#!/usr/bin/ruby

# FIXME the (morph (pos .) (concept . (sense . (word .)))) structure seems to be giving all the words to one morph, instead of copying the morph into each sense for each word to be added to it :-P

$: << ENV['TRIPS_BASE'] + '/etc/WordNetSQL'
require 'word_net_sql'

$pos2ss_type = Hash[*%w{noun n verb v adj a adv r}]
$pos2trips = Hash[*%w{noun n verb v adj adj adv adv}]

def write_pointer(out, pointer_name, target_synset_offset, target_ss_type, target_word_number)
  begin
    if (pointer_name == 'Pertains to noun / Derived from adjective')
      pointer_name =
	case (target_ss_type)
	  when 'n'; 'Pertains to'
	  when 'a','s'; 'Derived from'
	  else raise "Bogus WN pointer #{pointer_name} #{target_ss_type}%08d" % target_synset_offset
	end
    end 
    out.print("(> WN::#{pointer_name.gsub(/ /,'_')}")
    target_sense_keys = []
    if (target_word_number.nil?)
      out.print(" WN::%s%08d" % [target_ss_type, target_synset_offset])
    else
      target_sense_key =
        begin
	  WordNetSQL.query_first_value(
	    "SELECT sense_key FROM senses
	     WHERE synset_offset=? AND ss_type=? AND word_number=?;",
	    target_synset_offset, target_ss_type, target_word_number
	  )
	rescue
	  # if we didn't find the word_number in senses, it's probably a
	  # case-insensitive duplicate, and thus in the capitalization table
	  # instead
	  WordNetSQL.query_first_value(
	    "SELECT senses.sense_key
	     FROM senses JOIN capitalization USING (synset_offset, ss_type)
	     WHERE synset_offset=? AND ss_type=?
	       AND capitalization.word_number=?;",
	    target_synset_offset, target_ss_type, target_word_number)
	end
      unless (target_sense_keys.include?(target_sense_key))
        out.print(" WN::|#{target_sense_key}|")
	target_sense_keys.push(target_sense_key)
      end
    end
    out.puts(")")
  rescue Exception => e
    raise "Error writing DSL for pointer #{[pointer_name, target_synset_offset, target_ss_type, target_word_number].inspect}\n#{e.message}\n#{e.backtrace.join("\n")}\n"
  end
end

def write_dsl_for_ss_type(out, ss_type)
  WordNetSQL.db.execute("SELECT synset_offset, gloss FROM synsets WHERE ss_type=?;", ss_type) { |ss_row|
    synset_offset, gloss = *ss_row
    begin
      out.puts("  (concept WN::%s%08d" % [ss_type, synset_offset])
      # TODO separate examples and definitions, add tags, provenance?
      out.puts("    (definition (text #{gloss.inspect}))")
      WordNetSQL.db.execute("SELECT pointer_name, target_synset_offset, target_ss_type, target_word_number FROM pointers NATURAL JOIN pointer_symbols WHERE source_synset_offset=? AND source_ss_type=? AND source_word_number IS NULL;", synset_offset, ss_type) { |ptr_row|
	out.print("    ")
	write_pointer(out, *ptr_row)
      }
      WordNetSQL.db.execute("SELECT sense_number, word_number, sense_key, lemma FROM senses WHERE synset_offset=? AND ss_type=?;", synset_offset, ss_type) { |sense_row|
	sense_number, word_number, sense_key, lemma = *sense_row
	lemma_symbol = lemma.gsub(/'/, '^')
	word_symbols = lemma_symbol.split(/_/).collect { |ws|
	  ws = "|#{ws.upcase}|" unless (ws =~ /^[A-Za-z^][0-9A-Za-z^\.-]*$/)
	  ws
	}.join(' ')
	out.print <<EOP
    (sense WN::|#{sense_key}|
      (alias WN::#{lemma_symbol}.#{ss_type}.#{sense_number})
      (word (#{word_symbols}))
EOP
	# TODO look for word numbers in capitalization table too?
	WordNetSQL.db.execute("SELECT pointer_name, target_synset_offset, target_ss_type, target_word_number FROM pointers NATURAL JOIN pointer_symbols WHERE source_synset_offset=? AND source_ss_type=? AND source_word_number=?;", synset_offset, ss_type, word_number) { |ptr_row|
	  out.print("      ")
	  write_pointer(out, *ptr_row)
	}
	out.puts "      )"
      }
      out.puts "    )"
      out.puts
    rescue Exception => e
      raise "Error writing DSL for WN::#{ss_type}%08d:\n#{e.message}\n#{e.backtrace.join("\n")}\n" % synset_offset
    end
  }
end

%w{noun verb adj adv}.each { |pos|
  File.open("data.#{pos}.lisp", "w") { |out|
    out.puts ";;;; AUTOMATICALLY GENERATED"
    out.puts %Q{(provenance WordNet (version "3.0") (filename "data.#{pos}"))}
    out.puts "(morph (pos #{$pos2trips[pos]})"
    write_dsl_for_ss_type(out, $pos2ss_type[pos])
    write_dsl_for_ss_type(out, 's') if (pos == 'adj')
    out.puts "  )"
  }
}

