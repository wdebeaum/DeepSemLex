<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://exslt.org/strings"
  extension-element-prefixes="str">

<xsl:output method="html" />

<xsl:template match="/dsl">
 <html>
  <head>
   <title><xsl:value-of select="concept[1]/@name" /></title>
  </head>
  <body>
   <xsl:apply-templates select="concept[1]" mode="def" />
  </body>
 </html>
</xsl:template>

<xsl:template match="concept" mode="def">
 <xsl:variable name="name" select="@name" />
 <h1><xsl:value-of select="@name" /></h1>
 <h2>Relations</h2>
 <xsl:apply-templates select="relation" />
 <xsl:apply-templates select="sem-feats" />
 <xsl:apply-templates select="sem-frame" />
 <h2>Children</h2>
 <xsl:apply-templates select="/dsl/concept[contains(relation[@label='inherit'], $name)]" mode="ref"/>
 <h2>Senses</h2>
 <xsl:apply-templates select="/dsl/sense" />
</xsl:template>

<xsl:template match="concept" mode="ref">
 <xsl:text> </xsl:text><xsl:value-of select="@name" />
</xsl:template>

<xsl:template match="sense">
 <xsl:for-each select="morph/word">
  <h3>
   <xsl:value-of select="@first-word" />
   <xsl:value-of select="@remaining-words" />
   <xsl:if test="@particle">
    (<xsl:value-of select="@particle" />)
   </xsl:if>
  </h3>
 </xsl:for-each>
 <xsl:if test="not(morph/word)">
  <h3>MISSING WORD SPEC</h3> <!-- for now -->
 </xsl:if>
 POS: <xsl:value-of select="morph/pos/@pos" /><br/>
 Template: 
 <xsl:for-each select="str:tokenize(normalize-space(relation[@label='inherit']),' ')">
  <xsl:if test="substring(., string-length(.) - 5) = '-templ'">
   (<xsl:value-of select="." />)
  </xsl:if>
 </xsl:for-each>
 <xsl:for-each select="syntax/template-call">
  <xsl:text>(</xsl:text>
  <xsl:value-of select="@template" />
  <xsl:for-each select="@*[local-name() != 'template']">
   <xsl:text> :</xsl:text>
   <xsl:value-of select="local-name()" />
   <xsl:text> </xsl:text>
   <xsl:value-of select="." />
  </xsl:for-each>
  <xsl:text>)</xsl:text>
 </xsl:for-each>
 <br/>
 <xsl:if test="example">
  Examples:
  <ul>
   <xsl:for-each select="example">
    <li><xsl:value-of select="@text" /></li>
   </xsl:for-each>
  </ul>
 </xsl:if>
</xsl:template>

<xsl:template match="relation">
 <h3><xsl:value-of select="@label" /></h3>
 <ul>
  <xsl:for-each select="str:tokenize(normalize-space(), ' ')">
   <xsl:if test="substring(., string-length(.) - 5) != '-templ'">
    <li><xsl:value-of select="." /></li>
   </xsl:if>
  </xsl:for-each>
 </ul>
</xsl:template>

<xsl:template match="sem-feats">
 <h2>Semantic Features</h2>
   <!-- TODO -->
</xsl:template>

<xsl:template match="sem-frame">
 <h2>Semantic Frame</h2>
   <!-- TODO -->
</xsl:template>

</xsl:stylesheet>
