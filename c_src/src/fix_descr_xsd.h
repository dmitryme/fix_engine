/**
 * @file   fix_descr_xsd.h
 * @author Dmitry S. Melnikov, dmitryme@gmail.com
 * @date   Created on: 07/30/2012 02:40:30 PM
 */

#ifndef FIX_PARSER_FIX_DESCR_XSD_H
#define FIX_PARSER_FIX_DESCR_XSD_H

static char const* fix_xsd = {
"<?xml version='1.0'?>"
"<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'>"
""
"   <xs:simpleType name='RequiredType'>"
"      <xs:restriction base='xs:string'>"
"         <xs:pattern value='Y|N'/>"
"      </xs:restriction>"
"   </xs:simpleType>"
""
"   <xs:complexType name='FieldType'>"
"      <xs:attribute name='name' type='xs:string' use='required'/>"
"      <xs:attribute name='required' type='RequiredType' use='required'/>"
"   </xs:complexType>"
""
"   <xs:complexType name='ComponentType'>"
"      <xs:attribute name='name' type='xs:string' use='required'/>"
"      <xs:attribute name='required' type='RequiredType' use='required'/>"
"   </xs:complexType>"
""
"   <xs:complexType name='GroupType'>"
"      <xs:choice minOccurs='0' maxOccurs='unbounded'>"
"         <xs:element name='component' type='ComponentType' minOccurs='0'/>"
"         <xs:element name='field' type='FieldType' minOccurs='0'/>"
"         <xs:element name='group' type='GroupType' minOccurs='0'/>"
"      </xs:choice>"
"      <xs:attribute name='name' type='xs:string' use='required'/>"
"      <xs:attribute name='required' type='RequiredType' use='required'/>"
"   </xs:complexType>"
""
"   <xs:simpleType name='FieldValueType'>"
"      <xs:restriction base='xs:string'>"
"         <xs:pattern value='String|Length|Amt|Price|Char|Int|Currency|NumInGroup|LocalMktDate|Percentage|Boolean|MonthYear|Qty|Float|PriceOffset|UTCTimestamp|SeqNum|Data|Exchange|Country|MultipleValueString|MultipleCharValue|UTCTimeOnly|UTCDateOnly|DayOfMonth|TZTimeOnly|TZTimestamp|XMLData|Language|month-year|LocalMKDate'/>"
"      </xs:restriction>"
"   </xs:simpleType>"
""
"   <xs:element name='fix'>"
"      <xs:complexType>"
"         <xs:sequence>"
"            <xs:element name='messages'>"
"               <xs:complexType>"
"                  <xs:sequence>"
"                     <xs:element name='message' minOccurs='0' maxOccurs='unbounded'>"
"                        <xs:complexType>"
"                           <xs:choice minOccurs='0' maxOccurs='unbounded'>"
"                              <xs:element name='component' type='ComponentType' minOccurs='0'/>"
"                              <xs:element name='field' type='FieldType' minOccurs='0'/>"
"                              <xs:element name='group' type='GroupType' minOccurs='0'/>"
"                           </xs:choice>"
"                           <xs:attribute name='name' type='xs:string' use='required'/>"
"                           <xs:attribute name='type' type='xs:string' use='required'/>"
"                        </xs:complexType>"
"                     </xs:element>"
"                  </xs:sequence>"
"               </xs:complexType>"
"            </xs:element>"
"            <xs:element name='components'>"
"               <xs:complexType>"
"                  <xs:sequence>"
"                     <xs:element name='component' minOccurs='0' maxOccurs='unbounded'>"
"                        <xs:complexType>"
"                           <xs:choice minOccurs='0' maxOccurs='unbounded'>"
"                              <xs:element name='field' type='FieldType' minOccurs='0'/>"
"                              <xs:element name='component' type='ComponentType' minOccurs='0'/>"
"                              <xs:element name='group' type='GroupType' minOccurs='0'/>"
"                           </xs:choice>"
"                           <xs:attribute name='name' type='xs:string' use='required'/>"
"                        </xs:complexType>"
"                     </xs:element>"
"                  </xs:sequence>"
"               </xs:complexType>"
"            </xs:element>"
"            <xs:element name='fields'>"
"               <xs:complexType>"
"                  <xs:sequence>"
"                     <xs:element name='field' minOccurs='1' maxOccurs='unbounded'>"
"                        <xs:complexType>"
"                           <xs:sequence>"
"                              <xs:element name='value' minOccurs='0' maxOccurs='unbounded'>"
"                                 <xs:complexType>"
"                                    <xs:attribute name='enum' type='xs:string' use='required'/>"
"                                    <xs:attribute name='description' type='xs:string' use='required'/>"
"                                 </xs:complexType>"
"                              </xs:element>"
"                           </xs:sequence>"
"                           <xs:attribute name='number' type='xs:string' use='required'/>"
"                           <xs:attribute name='name' type='xs:string' use='required'/>"
"                           <xs:attribute name='type' type='FieldValueType' use='required'/>"
"                        </xs:complexType>"
"                     </xs:element>"
"                  </xs:sequence>"
"               </xs:complexType>"
"            </xs:element>"
"         </xs:sequence>"
"         <xs:attribute name='version' type='xs:string' use='required'/>"
"         <xs:attribute name='transport' type='xs:string' use='optional'/>"
"      </xs:complexType>"
"   </xs:element>"
"</xs:schema>"
};

#endif /* FIX_PARSER_FIX_DESCR_XSD_H */
