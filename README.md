# scar - Static Code Analysis for git Repositories

The main goal of scar is to analyze how a commit will affect a project. It tries to answer the following questions:

* Does a commit break or have the potential to break a projects public API?
  * Public API here can be an applications RESTful http api, the exposed functions/modules of a library etc...
* Which change in a commit's changesets broke the project?
  * Given test data and the commit ID scar should be able to analyze the commit and display the probability that a changeset broke the tests - the probability for all changesets should be displayed

