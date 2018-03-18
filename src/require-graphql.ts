const path = require("path")
const fs = require("fs")
const R = require("ramda")

module.exports = R.curry((module, filePath) => {
  const initPath = R.init(module.filename.split("/")).join("/")
  const wholePath = path.resolve(initPath, filePath)
  const completePath = `${wholePath}.graphql`

  return fs.readFileSync(completePath, "utf8")
})
