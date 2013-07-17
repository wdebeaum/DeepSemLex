#!/usr/bin/ruby

# FIXME the (morph (pos .) (concept . (sense . (word .)))) structure seems to be giving all the words to one morph, instead of copying the morph into each sense for each word to be added to it :-P

$: << ENV['TRIPS_BASE'] + '/etc/WordNetSQL'
require 'word_net_sql'

$pos2ss_type = Hash[*%w{noun n verb v adj a adv r}]
$pos2trips = Hash[*%w{noun n verb v adj adj adv adv}]

def write_dsl_for_ss_type(out, ss_type)
  WordNetSQL.db.execute("SELECT synset_offset, gloss FROM synsets WHERE ss_type=?;", ss_type) { |ss_row|
    synset_offset, gloss = *ss_row
    out.puts("  (concept WN::%s%08d" % [ss_type, synset_offset])
    # TODO separate examples and definitions, add tags, provenance?
    out.puts("    (definition (text #{gloss.inspect}))")
#      WordNetSQL.db.execute("SELECT source_synset_offset, source_ss_type, pointer_name, target_synset_offset, target_ss_type FROM pointers NATURAL JOIN pointer_symbols WHERE source_ss_type=?;", ss_type) { |ptr_row|
#        source_synset_offset, source_ss_type, pointer_name, target_synset_offset, target_ss_type = *ptr_row
#	# TODO
#      }
    WordNetSQL.db.execute("SELECT sense_number, sense_key, lemma FROM senses WHERE synset_offset=? AND ss_type=?;", synset_offset, ss_type) { |sense_row|
      sense_number, sense_key, lemma = *sense_row
      lemma_symbol = lemma.gsub(/'/, '^')
      word_symbols = lemma_symbol.split(/_/).collect { |ws|
        ws = "|#{ws.upcase}|" unless (ws =~ /^[A-Za-z^][0-9A-Za-z^\.-]*$/)
	ws
      }.join(' ')
      out.print <<EOP
    (sense WN::|#{sense_key}|
      (alias WN::#{lemma_symbol}.#{ss_type}.#{sense_number})
      (word (#{word_symbols}))
      )
EOP
    }
    out.puts "  )"
    out.puts
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

