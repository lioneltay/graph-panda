/**
 * All this module does is patch fields onto the existing schema, use it to add custom properties to the schema
 */

const assert = require("assert")

// properties on a type's _typeConfig that are allowed to be annotated
// instead of whitelisting properties we should blacklist reservered properties
const RESERVED_TYPE_PROPERTIES = [
  "name",
  "description",
  "astNode",
  "extensionASTNodes",
  "isTypeOf",
  "_typeConfig",
  "_interfaces",
  "_fields",
]
const validTypeAnnotation = prop => !RESERVED_TYPE_PROPERTIES.includes(prop)

const RESERVED_TYPE_CONFIG_PROPERTIES = [
  "name",
  "description",
  "fields",
  "interfaces",
  "astNode",
]
const validTypeConfigAnnotation = prop =>
  !RESERVED_TYPE_CONFIG_PROPERTIES.includes(prop)

// properties on a type field that are allowed to be annotated
// instead of whitelisting properties we should blacklist reservered properties
const RESERVED_FIELD_PROPERTIES = [
  "name",
  "description",
  "args",
  // "resolve",
  "deprecationReason",
  "astNode",
  "isDeprecated",
]
const validFieldAnnotation = prop => !RESERVED_FIELD_PROPERTIES.includes(prop)

const annotateSchema = (schema, typeAnnotations) => {
  for (const typeName in typeAnnotations) {
    const type = schema._typeMap[typeName]
    assert(type, `Type with name ${typeName} not found in schema.`)
    annotateType(type, typeAnnotations[typeName])
  }
}

const annotateType = (type, typeAnnotations) => {
  // Annotate typeConfig
  for (const property in typeAnnotations) {
    if (validTypeConfigAnnotation(property)) {
      type._typeConfig[property] = typeAnnotations[property]
    }
  }

  // Annotate each field of the type
  for (const fieldName in type._fields) {
    const field = type._fields[fieldName]
    assert(field, `Field "${fieldName}" not found in type "${type.name}".`)
    const fieldAnnotations = typeAnnotations.fields[fieldName]
    annotateField(field, fieldAnnotations)
  }
}

const annotateField = (field, fieldAnnotations = {}) => {
  for (const property in fieldAnnotations) {
    if (validFieldAnnotation(property)) {
      field[property] = fieldAnnotations[property]
    }
  }
}

module.exports = { annotateSchema }
