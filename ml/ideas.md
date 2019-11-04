# Ideas for privacy preserving Machine Learning

## Disambiguation

There are two different camps that understand very different
things under the term "privacy preserving machine learning".

One camp sees it as the purpose of privacy preserving machine
learning to hide the raw data from the entities training the model.
Whether a model overfits or otherwise contains model parameters
that would allow an analyst to deduce something about the presence
or absence of an individual in the training set is not seen as
a problem.

The second camp aims to find ways of performing machine learning
on data sets that are already available in their entirety, but
in such a way that the resulting model does not reveal something
about any single individual.

What follows are ideas for the second approach.

## Introduction

The process of anonymization inevitably results in loss of data.
Generally, it is the case that the higher the dimensionality of a
data set, the larger the distortion due to anonymization.

When training classifiers, or otherwise performing machine learning
tasks, it is common to want to use as much data as possible.
Especially in cases where an analyst does not know the data
being used in detail, using all the available dimensions is useful.
The machine learning algorithms is then left to deduce which parts
of the data are meaningful and which can be discarded. This approach
is incompatible with traditional anonymization.

## Possible approaches to ML on anonymized data

There are different approaches one can take to
machine learning when one has access to an anonymization system.

At a high level these are:

1. use off the shelf tools on anonymized data/query results
1. rebuild common algorithms to safely work on raw data
1. rebuild common algorithms to work around the limitations
  stemming from anonymization
1. pre-process the data before anonymization to make it
  less susceptible to data loss through anonymization
1. pre-process the data as part of the anonymization to
  yield richer anonymized results for subsequent machine learning
1. produce synthetic data for use by off the shelf tools


### Use off the shelf tools on anonymized results

This requires that the analyst has knowledge of which parts of
a data set might be useful, and which are not. This knowledge is
then used to only include relevant columns when anonymizing,
and to generalize the data where necessary to reduce the richness
of the data set in ways that minimally impacts the subsequent
machine learning.

This approach can be used both with static anonymization tools
and dynamic approaches to anonymization such as Diffix.

### Rebuild common algorithms to safely work on raw data

If one deeply understands the mathematics behind a machine
learning approach, one might reimplement it in such a way
that it is robust to outliers. This would entail not allowing
individuals to have an observable (or at least predictable)
effect on model parameters.

This approach has the benefit of allowing the full richness
of an unanonymized data set to be used for training, but
the drawback that each machine learning approach needs to be
reimplemented in an anonymizing form, and that it is currently
unknown how to do this while ensuring the model parameters remain
safe.

### Rebuild common algorithms to work around the limitations stemming from anonymization

Certain machine learning processes do not need high dimensional
data sets during training. The reason they do in their current
implementations is an artifact of the data _tending to be_
available in all its glory, and it leading to a simpler design.

A prime example of a machine learning process that does not
necessarily need access to all the data at the same time are
Random Forests. Each Random/Decision Tree, the constituent parts
of a Random Forest, can operate on a subset of the dimensions.
And the closer you are to the root of the tree, the fewer
dimensions you need to observe concurrently. In fact, at the
very root of the tree it suffices to have access to the column
you want to predict (the label) and one additional column
(the predictor).

This lends itself to implementations wherever richer anonymous
data is generated for training purposes dynamically as one moves
towards the leaves of a tree. At the point where the dimensionality
becomes so high that no further anonymous data can sensibly be
produced once can stop the training process.

Such an implementation depends being of an algorithm that can
operate on subsets of the data at a time, and on having access
to a system that can produce ever richer anonymous data sets on demand.
Diffix can serve this purpose.

### Pre-process the data before anonymization to make it less susceptible to data loss through anonymization

A trusted person or system with sufficient knowledge of a data set
could pre-process the data ahead of anonymization. This processing
could filter out extreme values, and result in a data set more
amenable to anonymization with less loss of signal.

Deep understanding of the impacts of generalization and bucketization
operations must be had by the trusted person, to avoid the data taking
a form that deceives, or otherwise prevents, the subsequent anonymization
step from anonymizing the data properly.

The benefit of such an approach is that the resulting anonymized
data set might be of a sufficient quality for general machine learning.

### Pre-process the data as part of the anonymization to yield richer anonymized results for subsequent machine learning

Much like in the previous step, pre-processing that is robust to
outliers and the impact of extreme values can be performed in an
automated step that makes data better suited for anonymization.

A general class of algorithms that can be used for this purpose are
those that reduce the dimensionality of a dataset. A concrete example
being principles components analysis (PCA). Robust forms of PCA exist.
By encoding the most salient information from a high number of columns
into a smaller set of columns, this information is easier to further
post-process and extract through an anonymization process.

After anonymization the principle components can be transformed back
into the original dimensions and subsequently used for machine learning.

### Produce synthetic data for use by off the shelf tools

High quality synthetic data allows for off the shelf machine
learning tools to be used. Producing high quality synthetic data
is all but easy, particularly if it is to be done in an automated
fashion.

## Prior work at Aircloak

- An [initial implementation of Random Forests](https://github.com/Aircloak/RandomForest)
  on top of Aircloak Insights was made the 22nd of May 2017. It is very much
  a work in progress
- A discussion on [Supporting ML scenarios through Aircloak](https://github.com/Aircloak/aircloak/issues/2481)
  from the 15th of March 2018 discusses the drawbacks and benefits of different approaches
- An issue from the 19th of March 2018 discusses [natively supporting PCA](https://github.com/Aircloak/aircloak/issues/2484)
  in Aircloak. Supporting algorithmic work can be found
  [here](https://github.com/Aircloak/statistical-science/tree/master/notebooks)


