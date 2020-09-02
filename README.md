# valohaiApi
This repository contains the source code for the valohaiApi R package, which provides an R interface to the valohai API.  
The main purpose for this package is to provide the user with an interface that removes the most common problems while communicating with the valohai API in addition to giving some common example usages for each function on each help page.

# Installation and setup
The package takes care of installing any and all dependencies necessary. In order to install the latest version as well as all dependencies and suggestions run
```R
devtools::install_github('bijaelo/valohaiApi')
```
The package provides auto-formatting of most return values as tibbles. For this to work the [`tidyr`](https://cran.r-project.org/package=tidyr) package should be installed. Simply installing the suggested dependencies for this to work as intended.


# R valohaiApi functions, descriptions and examples.
The R valohaiApi contains 5, each handling the the the input and output of a specific request type.

 - `getValohai(token, uri, query, id, ...)` 
 - `postValohai(token, uri, body, encoding, id, ...)`
 - `putValohai(token, uri, body, encoding, id, ...)`
 - `patchValohai(token, uri, body, encoding, id, ...)`
 - `deleteValohai(token, uri, body, encoding, id, ...)`

Refer to the [valohai API documentation](https://app.valohai.com/api/docs/) which states which type of calls hould be used for each type of request.  
Generally however, if you want to fetch some data, `valohaiGet` is the function to be used, while `valohaiPost` is used for creating something new, like an Execution, task, pipeline or user. `valohaiPut` and `valohaiPatch` often are synonymous and are used to alter deployments, users etc, while `valohaiDelete` is used to remove deployments, executions, etc.  
In addition to the above 5 functions `valohaiRequest(token, uri, body, query, encoding, id, ...)` provides a general interface to each type of request.

## General workflow
Before going in-depth with examples, I believe it is worthwhile to just briefly repeat how the general workflow works on valohai and how one interacts with the API. These are described more in depth in the [valohai core concepts](https://docs.valohai.com/core-concepts/what-is-valohai/).

When working in the valohai framework, we talk about a valohai environment as a folder (possibly a git repository) containing one or more folder, subfolders or any file types while also containing a [yaml file](https://docs.valohai.com/core-concepts/configuration-file/) describing how the folder should be interpreted by the valohai API. This file is created manually (or programmatically) outside the API framework and contains steps which are used to execute various steps in docker containers described in an image field.  
If enough support sprouts, a pet project might appear looking into creating a similar project as the python [valohai CLI](https://docs.valohai.com/valohai-cli/) and [valohai YAML](https://docs.valohai.com/core-concepts/configuration-file/) project with a focus on a standalone R interface.

As the api stands the usual learning path consists of 
 1. Looking through the [valohai API documentation](https://app.valohai.com/api/docs/) for the relevant call and it's parameters.
 2. Look at the help page for the specific function, checking whether an active example is available (especially useful for things such as pipelines)
 3. Use the acquired knowledge to interface with one of the 5 functions.

## Examples
A simple and common example usage of the valohai API is to have a more complex task that needs to be executed, or to generate a large pipeline. 
