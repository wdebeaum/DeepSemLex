;;;;
;;;; W::phosphorilate and other post translational modifications
;;;;

; note: there are two separate define-words in here (when changing templates, need to change both)

(define-words :pos W::V :templ agent-affected-loc-optional-templ
 :words (
  (W::phosphorilate
   (wordfeats (W::morph (:forms (-vb) :nom w::phosphorilation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::phosphorylation)
     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
    ))

  (W::ribosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::ribosylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ribosylation)
     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
    ))

  ;; and here's an example of a multi-word case
   ((W::gamma W::carboxylize)
    (wordfeats (W::morph (:forms (-vb) :nom (w::gamma W::carboxilation))))
    (SENSES
     ((LF-PARENT ONT::post-translational-modification))
     ))

  ;; others go here

  ;; # PTMs involving addition by an enzyme in vivo
  ;; ## PTMs involving addition of hydrophobic groups for membrane localization
  (W::myristoylate
  ;; I've seen this used, but it is uncommon.  Mostly the nominalized form
   (wordfeats (W::morph (:forms (-vb) :nom w::myristoylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::palmitoylate
  ;; I've seen this used, but it is uncommon.  Mostly the nominalized form
   (wordfeats (W::morph (:forms (-vb) :nom w::palmitoylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::isoprenylate
  ;; I've seen this used, but it is uncommon.  Mostly the nominalized form
   (wordfeats (W::morph (:forms (-vb) :nom w::isoprenylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::prenylate
  ;; synonym for isoprenylate?
   (wordfeats (W::morph (:forms (-vb) :nom w::prenylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::farnesylate
  ;; child of isoprenylate?
   (wordfeats (W::morph (:forms (-vb) :nom w::farnesylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::farnesylation)
     (templ agent-affected-loc-optional-templ))
    ))

  (W::geranylgeranylate
  ;; child of isoprenylate?
   (wordfeats (W::morph (:forms (-vb) :nom w::geranylgeranylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::glypiate
   (wordfeats (W::morph (:forms (-vb) :nom w::glypiation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; ## PTMs involving addition of cofactors for enhanced enzymatic activity

  (W::lipoylate
   (wordfeats (W::morph (:forms (-vb) :nom w::lipoylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping "flavin moiety"
  ;; skipping "heme C attachment"

  (W::phosphopantetheinylate
   (wordfeats (W::morph (:forms (-vb) :nom w::phosphopantetheinylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping retinylidene Schiff base formation

  ;; ## PTMs involving unique modifications of translation factors

  ;; skipping diphthamide formation
  ;; skipping ethanolamine phosphoglycerol attachment
  ;; skipping hypusine formation

  ;; ## PTMs involving addition of smaller chemical groups


  (W::acylate
   (wordfeats (W::morph (:forms (-vb) :nom w::acylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))


  (W::acetylate
    ;; Child of acylate
   (wordfeats (W::morph (:forms (-vb) :nom w::acetylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ACETYLATION)
     (templ agent-affected-loc-optional-templ))
    ))

;  (W::deacetylation
;    ;; reverse of acetylation
;   (wordfeats (W::morph (:forms (-vb) :nom w::deacetylation)))
;   (SENSES
;    ((LF-PARENT ONT::post-translational-modification))
;    ))

  (W::formylate
    ;; Child of acylate
   (wordfeats (W::morph (:forms (-vb) :nom w::formylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::alkylate
   (wordfeats (W::morph (:forms (-vb) :nom w::alkylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::methylate
    ;; Child of alkylate
   (wordfeats (W::morph (:forms (-vb) :nom w::methylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::METHYLATION)
     (templ agent-affected-loc-optional-templ))
    ))

;  (W::demethylation
;    ;; reverse of methylation
;   (wordfeats (W::morph (:forms (-vb) :nom w::demethylation)))
;   (SENSES
;    ((LF-PARENT ONT::post-translational-modification))
;    ))

  ;; skipping amide bond formation

  (W::amidate
    ;; child of "amide bond formation"
   (wordfeats (W::morph (:forms (-vb) :nom w::amidation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping "amino acid addition" child of "amide bond formation"

  (W::arginylate
    ;; child of "amino acid addition"
   (wordfeats (W::morph (:forms (-vb) :nom w::arginylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::polyglutamylate
    ;; child of "amino acid addition"
   (wordfeats (W::morph (:forms (-vb) :nom w::polyglutamylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::polyglycylate
    ;; child of "amino acid addition"
   (wordfeats (W::morph (:forms (-vb) :nom w::polyglycylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::butyrylate
   (wordfeats (W::morph (:forms (-vb) :nom w::butyrylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::gamma-carboxylate
   (wordfeats (W::morph (:forms (-vb) :nom w::gamma-carboxylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::glycosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::glycosylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::glycosylation)
     (templ agent-affected-loc-optional-templ))
    ))

  (W::polysialylate
    ;; child of glycosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::polysialylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::malonylate
   (wordfeats (W::morph (:forms (-vb) :nom w::malonylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::hydroxylate
   (wordfeats (W::morph (:forms (-vb) :nom w::hydroxylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::hydroxylation)
     (templ agent-affected-loc-optional-templ))
    ))

  (W::iodinate
   (wordfeats (W::morph (:forms (-vb) :nom w::iodination)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))


  ;; skipping nucleotide addition

  (W::oxidate
   (wordfeats (W::morph (:forms (-vb) :nom w::oxidation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))


  ;; skipping phosphate ester (O-linked) or phosphoramidate (N-linked) formation

  (W::phosphorylate
  ;; child of phosphate ester (O-linked) or phosphoramidate (N-linked) formation
   (wordfeats (W::morph (:forms (-vb) :nom w::phosphorylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::phosphorylation)
     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
    ))

;  (W::dephosphorylate
;   (wordfeats (W::morph (:forms (-vb) :nom w::dephosphorylation :nomobjpreps (w::on))))
;   (SENSES
;    ((LF-PARENT ONT::post-translational-modification)
;     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
;    ))

  (W::adenylylate
  ;; child of phosphate ester (O-linked) or phosphoramidate (N-linked) formation
   (wordfeats (W::morph (:forms (-vb) :nom w::adenylylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::propionylate
   (wordfeats (W::morph (:forms (-vb) :nom w::propionylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping pyroglutamate formation

  (W::S-glutathionylate
   (wordfeats (W::morph (:forms (-vb) :nom w::S-glutathionylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::S-nitrosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::S-nitrosylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::succinylate
   (wordfeats (W::morph (:forms (-vb) :nom w::succinylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::sulfate
   (wordfeats (W::morph (:forms (-vb) :nom w::sulfation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; ## PTMs involving non-enzymatic additions in vivo

  (W::glycate
   (wordfeats (W::morph (:forms (-vb) :nom w::glycation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; ### PTMs involving non-enzymatic additions in vitro
  (W::biotinylate
   (wordfeats (W::morph (:forms (-vb) :nom w::biotinylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::pegylate
   (wordfeats (W::morph (:forms (-vb) :nom w::pegylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; # PTMs involving addition of other proteins or peptides

  (W::ISGylate
   (wordfeats (W::morph (:forms (-vb) :nom w::ISGylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::sumoylate
   (wordfeats (W::morph (:forms (-vb) :nom w::sumoylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::sumoylation)
     (templ agent-affected-loc-optional-templ))
    ))

  (W::ubiquitinate
   (wordfeats (W::morph (:forms (-vb) :nom w::ubiquitination :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ubiquitination)
     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
    ))

  (W::ubiquinate
   (wordfeats (W::morph (:forms (-vb) :nom w::ubiquination :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ubiquitination)
     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
    ))

;  (W::monoubiquitinate
;   (wordfeats (W::morph (:forms (-vb) :nom w::monoubiquitination :nomobjpreps (w::on))))
;   (SENSES
;    ((LF-PARENT ONT::monoubiquitination)
;     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
;    ))

  (W::ubiquitylate
   (wordfeats (W::morph (:forms (-vb) :nom w::ubiquitylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::Neddylate
   (wordfeats (W::morph (:forms (-vb) :nom w::Neddylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::Pupylate
   (wordfeats (W::morph (:forms (-vb) :nom w::Pupylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; # PTMs involving changing the chemical nature of amino acids

  (W::citrullinate
   (wordfeats (W::morph (:forms (-vb) :nom w::citrullination)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::deiminate
  ;; synomyn of citrullinate?
   (wordfeats (W::morph (:forms (-vb) :nom w::deimination)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::deamidate
   (wordfeats (W::morph (:forms (-vb) :nom w::deamidation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::eliminylate
   (wordfeats (W::morph (:forms (-vb) :nom w::eliminylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::carbamylate
   (wordfeats (W::morph (:forms (-vb) :nom w::carbamylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; PTMs involving structural changes

  ;; skipping disulfide bridges
  ;; skipping proteolytic cleavage

  (W::racemizate
   (wordfeats (W::morph (:forms (-vb) :nom w::racemization)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))
))



(define-words :pos W::V :templ agent-affected-loc-optional-templ
 :words (
  (W::phosphorilate
   (wordfeats (W::morph (:forms (-vb) :nom w::phosphorilation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::phosphorylation)
     )  ;; subcat for location to improve attachment decisions
    ))

  (W::ribosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::ribosylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ribosylation)
     )  ;; subcat for location to improve attachment decisions
    ))

  ;; and here's an example of a multi-word case
   ((W::gamma W::carboxylize)
    (wordfeats (W::morph (:forms (-vb) :nom (w::gamma W::carboxilation))))
    (SENSES
     ((LF-PARENT ONT::post-translational-modification))
     ))

  ;; others go here

  ;; # PTMs involving addition by an enzyme in vivo
  ;; ## PTMs involving addition of hydrophobic groups for membrane localization
  (W::myristoylate
  ;; I've seen this used, but it is uncommon.  Mostly the nominalized form
   (wordfeats (W::morph (:forms (-vb) :nom w::myristoylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::palmitoylate
  ;; I've seen this used, but it is uncommon.  Mostly the nominalized form
   (wordfeats (W::morph (:forms (-vb) :nom w::palmitoylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::isoprenylate
  ;; I've seen this used, but it is uncommon.  Mostly the nominalized form
   (wordfeats (W::morph (:forms (-vb) :nom w::isoprenylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::prenylate
  ;; synonym for isoprenylate?
   (wordfeats (W::morph (:forms (-vb) :nom w::prenylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::farnesylate
  ;; child of isoprenylate?
   (wordfeats (W::morph (:forms (-vb) :nom w::farnesylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::farnesylation)
     )
    ))

  (W::geranylgeranylate
  ;; child of isoprenylate?
   (wordfeats (W::morph (:forms (-vb) :nom w::geranylgeranylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::glypiate
   (wordfeats (W::morph (:forms (-vb) :nom w::glypiation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; ## PTMs involving addition of cofactors for enhanced enzymatic activity

  (W::lipoylate
   (wordfeats (W::morph (:forms (-vb) :nom w::lipoylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping "flavin moiety"
  ;; skipping "heme C attachment"

  (W::phosphopantetheinylate
   (wordfeats (W::morph (:forms (-vb) :nom w::phosphopantetheinylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping retinylidene Schiff base formation

  ;; ## PTMs involving unique modifications of translation factors

  ;; skipping diphthamide formation
  ;; skipping ethanolamine phosphoglycerol attachment
  ;; skipping hypusine formation

  ;; ## PTMs involving addition of smaller chemical groups


  (W::acylate
   (wordfeats (W::morph (:forms (-vb) :nom w::acylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))


  (W::acetylate
    ;; Child of acylate
   (wordfeats (W::morph (:forms (-vb) :nom w::acetylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ACETYLATION)
     )
    ))

;  (W::deacetylation
;    ;; reverse of acetylation
;   (wordfeats (W::morph (:forms (-vb) :nom w::deacetylation)))
;   (SENSES
;    ((LF-PARENT ONT::post-translational-modification))
;    ))

  (W::formylate
    ;; Child of acylate
   (wordfeats (W::morph (:forms (-vb) :nom w::formylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::alkylate
   (wordfeats (W::morph (:forms (-vb) :nom w::alkylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::methylate
    ;; Child of alkylate
   (wordfeats (W::morph (:forms (-vb) :nom w::methylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::METHYLATION)
     )
    ))

;  (W::demethylation
;    ;; reverse of methylation
;   (wordfeats (W::morph (:forms (-vb) :nom w::demethylation)))
;   (SENSES
;    ((LF-PARENT ONT::post-translational-modification))
;    ))

  ;; skipping amide bond formation

  (W::amidate
    ;; child of "amide bond formation"
   (wordfeats (W::morph (:forms (-vb) :nom w::amidation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping "amino acid addition" child of "amide bond formation"

  (W::arginylate
    ;; child of "amino acid addition"
   (wordfeats (W::morph (:forms (-vb) :nom w::arginylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::polyglutamylate
    ;; child of "amino acid addition"
   (wordfeats (W::morph (:forms (-vb) :nom w::polyglutamylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::polyglycylate
    ;; child of "amino acid addition"
   (wordfeats (W::morph (:forms (-vb) :nom w::polyglycylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::butyrylate
   (wordfeats (W::morph (:forms (-vb) :nom w::butyrylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::gamma-carboxylate
   (wordfeats (W::morph (:forms (-vb) :nom w::gamma-carboxylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::glycosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::glycosylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::glycosylation)
     )
    ))

  (W::polysialylate
    ;; child of glycosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::polysialylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::malonylate
   (wordfeats (W::morph (:forms (-vb) :nom w::malonylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::hydroxylate
   (wordfeats (W::morph (:forms (-vb) :nom w::hydroxylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::hydroxylation)
     )
    ))

  (W::iodinate
   (wordfeats (W::morph (:forms (-vb) :nom w::iodination)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))


  ;; skipping nucleotide addition

  (W::oxidate
   (wordfeats (W::morph (:forms (-vb) :nom w::oxidation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))


  ;; skipping phosphate ester (O-linked) or phosphoramidate (N-linked) formation

  (W::phosphorylate
  ;; child of phosphate ester (O-linked) or phosphoramidate (N-linked) formation
   (wordfeats (W::morph (:forms (-vb) :nom w::phosphorylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::phosphorylation)
     )  ;; subcat for location to improve attachment decisions
    ))

;  (W::dephosphorylate
;   (wordfeats (W::morph (:forms (-vb) :nom w::dephosphorylation :nomobjpreps (w::on))))
;   (SENSES
;    ((LF-PARENT ONT::post-translational-modification)
;     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
;    ))

  (W::adenylylate
  ;; child of phosphate ester (O-linked) or phosphoramidate (N-linked) formation
   (wordfeats (W::morph (:forms (-vb) :nom w::adenylylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::propionylate
   (wordfeats (W::morph (:forms (-vb) :nom w::propionylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; skipping pyroglutamate formation

  (W::S-glutathionylate
   (wordfeats (W::morph (:forms (-vb) :nom w::S-glutathionylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::S-nitrosylate
   (wordfeats (W::morph (:forms (-vb) :nom w::S-nitrosylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::succinylate
   (wordfeats (W::morph (:forms (-vb) :nom w::succinylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::sulfate
   (wordfeats (W::morph (:forms (-vb) :nom w::sulfation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; ## PTMs involving non-enzymatic additions in vivo

  (W::glycate
   (wordfeats (W::morph (:forms (-vb) :nom w::glycation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; ### PTMs involving non-enzymatic additions in vitro
  (W::biotinylate
   (wordfeats (W::morph (:forms (-vb) :nom w::biotinylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::pegylate
   (wordfeats (W::morph (:forms (-vb) :nom w::pegylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; # PTMs involving addition of other proteins or peptides

  (W::ISGylate
   (wordfeats (W::morph (:forms (-vb) :nom w::ISGylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::sumoylate
   (wordfeats (W::morph (:forms (-vb) :nom w::sumoylation :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::sumoylation)
     )
    ))

  (W::ubiquitinate
   (wordfeats (W::morph (:forms (-vb) :nom w::ubiquitination :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ubiquitination)
     )  ;; subcat for location to improve attachment decisions
    ))

  (W::ubiquinate
   (wordfeats (W::morph (:forms (-vb) :nom w::ubiquination :nomobjpreps (w::on w::of))))
   (SENSES
    ((LF-PARENT ONT::ubiquitination)
     )  ;; subcat for location to improve attachment decisions
    ))

;  (W::monoubiquitinate
;   (wordfeats (W::morph (:forms (-vb) :nom w::monoubiquitination :nomobjpreps (w::on))))
;   (SENSES
;    ((LF-PARENT ONT::monoubiquitination)
;     (templ agent-affected-loc-optional-templ))  ;; subcat for location to improve attachment decisions
;    ))

  (W::ubiquitylate
   (wordfeats (W::morph (:forms (-vb) :nom w::ubiquitylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::Neddylate
   (wordfeats (W::morph (:forms (-vb) :nom w::Neddylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::Pupylate
   (wordfeats (W::morph (:forms (-vb) :nom w::Pupylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; # PTMs involving changing the chemical nature of amino acids

  (W::citrullinate
   (wordfeats (W::morph (:forms (-vb) :nom w::citrullination)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::deiminate
  ;; synomyn of citrullinate?
   (wordfeats (W::morph (:forms (-vb) :nom w::deimination)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::deamidate
   (wordfeats (W::morph (:forms (-vb) :nom w::deamidation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::eliminylate
   (wordfeats (W::morph (:forms (-vb) :nom w::eliminylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  (W::carbamylate
   (wordfeats (W::morph (:forms (-vb) :nom w::carbamylation)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))

  ;; PTMs involving structural changes

  ;; skipping disulfide bridges
  ;; skipping proteolytic cleavage

  (W::racemizate
   (wordfeats (W::morph (:forms (-vb) :nom w::racemization)))
   (SENSES
    ((LF-PARENT ONT::post-translational-modification))
    ))
))
