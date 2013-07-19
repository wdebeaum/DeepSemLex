<?xml version="1.0"?>
<stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/XSL/Transform"
  xmlns:fn="http://framenet.icsi.berkeley.edu"
  >

<!-- framenet-frame-to-dsl.xsl - convert FrameNet frame/*.xml files to DeepSemLex Lisp data files -->

<output method="text" />

<template match="@*|text()" />

<!-- emit a newline and an appropriate level of indentation -->
<template name="nl-indent">
 <text>
</text>
 <for-each select="ancestor::fn:frame | ancestor::fn:FE | ancestor::fn:lexUnit">
  <text>  </text>
 </for-each>
</template>

<template match="/fn:frame">
 <text>;;; AUTOMATICALLY GENERATED
(provenance FrameNet (filename "frame/</text>
 <value-of select="@name" />
 <text>.xml"))
(sem-frame FN::</text>
 <value-of select="@name" />
 <text>
  (alias FN::f</text>
 <value-of select="@ID" />
 <text>)
  </text>
 <apply-templates />
 <text>
  )
</text>
</template>

<!-- template match="definition[parent::frame]">
 TODO convert this to DSL definition with tags converted to lattice?
</template -->

<template match="fn:definition">
 <variable name="nl-indent">
  <call-template name="nl-indent" />
 </variable>
 <for-each select="tokenize(., '&#x0a;')">
  <value-of select="$nl-indent" />
  <text>;; </text>
  <value-of select="replace(replace(replace(., '&amp;lt;', '&lt;'), '&amp;gt;', '&gt;'), '&amp;amp;', '&amp;')" />
 </for-each>
</template>

<template match="fn:FE">
 <call-template name="nl-indent" />
 <text>(FN::</text>
 <value-of select="@name" />
 <choose>
  <when test="count(fn:semType) = 0"><text> t</text></when>
  <when test="count(fn:semType) = 1">
   <text> FN::</text>
   <value-of select="fn:semType/@name" />
  </when>
  <otherwise>
   <text>(or</text>
   <for-each select="fn:semType">
    <text> FN::</text>
    <value-of select="@name" />
   </for-each>
   <text>)</text>
  </otherwise>
 </choose>
 <if test="@coreType != 'Core'">
  <text> optional</text> <!-- I think? -->
 </if>
 <text>)</text>
</template>

<template match="fn:frameRelation[@type='Inherits from' and fn:relatedFrame]">
 <call-template name="nl-indent" />
 <text>(subtype-of</text> <!-- I think? -->
 <for-each select="fn:relatedFrame">
  <text> FN::</text>
  <value-of select="." />
 </for-each>
 <text>)</text>
</template>

<!-- TODO other frameRelations -->

<template match="fn:lexUnit">
 <call-template name="nl-indent" />
 <text>(sense FN::</text>
 <value-of select="parent::fn:frame/@name" />
 <text>.</text>
 <value-of select="replace(replace(replace(replace(@name, '\(', '['), '\)', ']'), '''', '^'), '[\s&quot;`,]', '_')" />
 <call-template name="nl-indent" />
 <text>  (alias FN::lu</text>
 <value-of select="@ID" />
 <text>)</text>
 <call-template name="nl-indent" />
 <text>  (morph (pos </text>
 <choose>
  <when test="@POS='A'"><text>ADJ</text></when>
  <when test="@POS='AVP'"><text>ADV</text></when>
  <when test="@POS='PRON'"><text>PRO</text></when>
  <when test="@POS='NUM'"><text>NUMBER</text></when>
  <when test="@POS='INTJ'"><text>UTTWORD</text></when>
  <otherwise><value-of select="@POS" /></otherwise>
  <!-- not sure about C, SCON, CCON -->
 </choose>
 <text>) (word </text>
 <if test="count(fn:lexeme) > 1"><text>(</text></if>
 <for-each select="fn:lexeme">
  <if test="position() gt 1 and @breakBefore='false'"><text> </text></if>
  <if test="@breakBefore='true'"><text>(</text></if> <!-- hopefully 0 or 1 -->
  <value-of select="@name" />
 </for-each>
 <if test="fn:lexeme[@breakBefore='true']"><text>)</text></if>
 <if test="count(fn:lexeme) > 1"><text>)</text></if>
 <text>))</text>
 <apply-templates />
 <call-template name="nl-indent" />
 <text>  )</text>
</template>

<template match="fn:definition[parent::fn:lexUnit]">
 <call-template name="nl-indent" />
 <text>(definition (text "</text>
 <value-of select="substring-after(., ': ')" />
 <text>")</text>
 <if test="not(starts-with(., 'FN: '))">
  <text> (provenance </text>
  <value-of select="substring-before(., ': ')" />
  <text>)</text>
 </if>
 <text>)</text>
</template>

</stylesheet>

