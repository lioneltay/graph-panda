var R = require("ramda");
var getFragment = function (info, fragmentName) {
    return info.fragments[fragmentName];
};
var getFragmentFields = function (info, fragmentName) {
    return extractFieldsFromSelections(info, getFragment(info, fragmentName).selectionSet.selections);
};
var extractFieldsFromSelections = function (info, selections) {
    return selections.reduce(function (fields, item) {
        var kind = item.kind, name = item.name;
        if (kind === "Field") {
            return fields.concat(name.value);
        }
        if (kind === "FragmentSpread") {
            return fields.concat(getFragmentFields(info, name.value));
        }
        return fields;
    }, []);
};
/**
 * Returns the requested fields at the given node
 */
var getRequestedFields = function (resolveInfo) {
    return R.uniq(extractFieldsFromSelections(resolveInfo, resolveInfo.fieldNodes[0].selectionSet.selections));
};
module.exports = {
    getRequestedFields: getRequestedFields,
};
//# sourceMappingURL=getRequestedFields.js.map