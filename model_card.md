# Model Card: {ambulance-DES}


## Model Details

The implementation of ambulance-DES within this repository is the public facing version relating to a proof of concept piece of work conducted in NHS England from June-July 2022.  The model itself is a discrete event simulation using a trajectory to define possible routes.

## Model Use

### Intended Use

This model is intended as a proof of concept for understanding the non-linear relation between ambulance availability and response times, including the effect of creating trade-offs in resource use between handover and mobile job cycle time.

### Out-of-Scope Use Cases

This model is a very simple representation of the ambulance system, meant as a proof-of-concept and toy model to elicit discussion with stakeholders and aid interpretation of non-linearities and trade-offs rather than created for direct operational use and productionisation.  Escalation or human behaviour characteristics are included in a limited scope in the model (interventions), but will not cover all cases.  Job cycle times are stylistic. Only Category 2 demand is included. ED is out of scope: the handover to ED is implemented as a delay rather than a queue for a capacity constrained resource.

Please consult the subsequent project repository (https://github.com/nhsengland/AmbModelOpen) and documentation (https://nhsengland.github.io/AmbModelOpen/) for wider scope use cases.
