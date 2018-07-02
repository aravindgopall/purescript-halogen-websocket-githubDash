 path = require("path");
module.exports = {
        entry: {
                    main: "./index.js"
                },
        output: {
                    path: path.join(__dirname, "out"),
                    filename: "webhook.js"
                },
        target: "node"
};
