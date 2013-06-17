<?xml version="1.0"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- verbnet-to-dsl.xsl - convert VerbNet 3.2 XML files to DeepSemLex Lisp data files -->

<xsl:output method="text" />

<xsl:template match="@*|text()" />

<!-- emit a newline and an appropriate level of indentation -->
<xsl:template name="nl-indent">
 <xsl:text>
</xsl:text>
 <xsl:for-each select="ancestor::VNCLASS | ancestor::VNSUBCLASS | ancestor::THEMROLES | ancestor::FRAMES[count(FRAME) > 1] | ancestor::EXAMPLES | ancestor::SYNTAX | ancestor::FRAME | ancestor::SEMANTICS">
  <xsl:text>  </xsl:text>
 </xsl:for-each>
</xsl:template>

<xsl:template match="/VNCLASS">
 <xsl:text>;;; AUTOMATICALLY GENERATED
</xsl:text>
 <xsl:text>(concept VN::</xsl:text>
 <xsl:value-of select="@ID" />
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )

</xsl:text>
</xsl:template>

<xsl:template match="VNSUBCLASS">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(concept VN::</xsl:text>
 <xsl:value-of select="@ID" />
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="MEMBERS">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(overlap</xsl:text>
 <xsl:apply-templates />
 <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="MEMBER[@wn != '']">
 <xsl:text> WN::|</xsl:text>
 <xsl:value-of select="replace(@wn, ' ', '::| WN::|')" />
 <xsl:text>::|</xsl:text>
</xsl:template>

<xsl:template match="THEMROLES">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(sem-frame</xsl:text>
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="THEMROLE">
 <xsl:variable name="type" select="@type" />
 <xsl:call-template name="nl-indent" />
 <xsl:text>(</xsl:text>
 <xsl:value-of select="$type" />
 <xsl:apply-templates />
 <xsl:choose>
  <xsl:when test="not(../../FRAMES/FRAME/SYNTAX/*[@value=$type] | ../../FRAMES/FRAME/SEMANTICS[@value=$type or @value=concat('?',$type)])">
   <xsl:text> unused</xsl:text>
  </xsl:when>
  <xsl:when test="../../FRAMES/FRAME/SYNTAX[not(child::node()[@value=$type])]">
   <xsl:text> optional</xsl:text>
  </xsl:when>
 </xsl:choose>
 <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="SELRESTRS">
 <xsl:choose>
  <xsl:when test="@logic='or'">
   <xsl:text> (or</xsl:text>
   <xsl:if test="SELRESTR">
    <xsl:text> (sem-feats</xsl:text>
    <xsl:apply-templates select="SELRESTR" />
    <xsl:text>)</xsl:text>
   </xsl:if>
   <xsl:apply-templates select="SELRESTRS" />
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:when test="SELRESTR">
   <xsl:text> (sem-feats</xsl:text>
   <xsl:apply-templates />
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:text> t</xsl:text>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- TODO translate these to be more TRIPS-like? -->
<xsl:template match="SELRESTR">
 <xsl:text> (</xsl:text>
 <xsl:value-of select="@type" />
 <xsl:text> </xsl:text>
 <xsl:value-of select="@Value" />
 <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="FRAMES">
 <xsl:choose>
  <xsl:when test="count(FRAME) > 1">
   <xsl:call-template name="nl-indent" />
   <xsl:text>(or</xsl:text>
   <xsl:apply-templates />
   <xsl:call-template name="nl-indent" />
   <xsl:text>  )</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:apply-templates />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="FRAME">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(concept ; ?</xsl:text>
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="SYNTAX">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(syn-sem</xsl:text>
 <xsl:for-each select="../EXAMPLES/EXAMPLE">
  <xsl:call-template name="nl-indent" />
  <xsl:text>(example (source vn) (text "</xsl:text>
  <xsl:value-of select="." />
  <xsl:text>"))</xsl:text>
 </xsl:for-each>
 <xsl:apply-templates select="NP" />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="SEMANTICS">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(predicate (and</xsl:text>
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  ))</xsl:text>
</xsl:template>

<xsl:template match="PRED">
 <xsl:call-template name="nl-indent" />
 <xsl:if test="@bool='!'"><xsl:text>(not </xsl:text></xsl:if>
 <xsl:text>(</xsl:text>
 <xsl:value-of select="@value" />
 <xsl:apply-templates />
 <xsl:text>)</xsl:text>
 <xsl:if test="@bool='!'"><xsl:text>)</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="ARG">
 <xsl:text> </xsl:text>
 <xsl:choose>
  <xsl:when test="@type='Constant'">
   <xsl:value-of select="@value" />
  </xsl:when>
  <xsl:when test="@type='Event' and contains(@value, '(')">
   <xsl:value-of select="replace(@value, '(\w+)\(E\)', '($1 ?E)')" />
  </xsl:when>
  <xsl:otherwise> <!-- Event, ThemRole, VerbSpecific -->
   <xsl:if test="not(starts-with(@value, '?'))"><xsl:text>?</xsl:text></xsl:if>
   <xsl:value-of select="@value" />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- TODO SYNRESTRs, esp. those that identify an "NP" as really a to-infinitive complement or something -->

<xsl:template match="NP">
 <xsl:call-template name="nl-indent" />
 <xsl:choose>
  <xsl:when test="not(preceding-sibling::node())">
   <xsl:text>(lsubj NP </xsl:text>
  </xsl:when>
  <xsl:when test="local-name(preceding-sibling::node()[1])='PREP'">
   <xsl:text>(comp </xsl:text>
   <xsl:choose>
    <xsl:when test="not(preceding-sibling::PREP/@value)">
     <xsl:text>PP</xsl:text>
     <!-- TODO SELRESTRS? -->
    </xsl:when>
    <xsl:when test="contains(preceding-sibling::PREP[1]/@value, ' ')">
     <xsl:text>(PP (or </xsl:text>
     <xsl:value-of select="preceding-sibling::PREP[1]/@value" />
     <xsl:text>))</xsl:text>
    </xsl:when>
    <xsl:otherwise>
     <xsl:text>(PP </xsl:text>
     <xsl:value-of select="preceding-sibling::PREP[1]/@value" />
     <xsl:text>)</xsl:text>
    </xsl:otherwise>
   </xsl:choose>
   <xsl:text> </xsl:text>
  </xsl:when>
  <xsl:when test="local-name(preceding-sibling::node()[1])='VERB' and
                  local-name(following-sibling::node()[1])='NP'">
   <xsl:text>(liobj NP </xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:text>(lobj NP </xsl:text>
  </xsl:otherwise>
 </xsl:choose>
 <xsl:value-of select="@value" />
 <xsl:text>)</xsl:text>
</xsl:template>

<!-- TODO ADJ ADV LEX -->

<!-- TODO convert SEMANTICS/PRED/ARGS/ARG to LF terms? -->

</xsl:stylesheet>
