Configuring outputs using a hierarchical structure. The hierarchy has 3 levels (with 1 being the highest):

1. Module e.g. Biogeophysics
2. Group e.g. canopy, soil, nitrogen. Each group belongs to a module.
3. Variable e.g. GPP, soil_moisture, labile_carbon. Each variable belongs to a group.

The lowest level specification always take precedence. So if a group has specified "daily max" and "daily min", but a variable within that group has specified "3hourly mean", the "3hourly mean" will be applied.

## Specified methods

The `.conf` files are simple: A specification of the level, with the `<method>:<frequency>` methods to apply.

```
module | <agg_method>:<frequency>, <agg_method>:<frequency>, ... ! Any number of method specifications
group:<group_name> | <agg_method>:<frequency>, <agg_method>:<frequency>, ... 
variable:<variable_name> | <agg_method>:<frequency>, <agg_method>:<frequency>, ...
```

There is one special instance, when we want to deactive a level. Here we just specify `off` like:

```
module | off
```

This would switch off all output for the given module. You can switch off a whole module, then only activate a couple of variables by specifying those variables in the file.

The intention is to have only a single module's specification in each file, hence the `module` line doesn't specify a name.

