// -----------------------------------------------------------------------------
// --------------- CONFIGURATION BLOCK -----------------------------------------
// -----------------------------------------------------------------------------
// INSTRUCTIONS:
//   You can control parameters to your build process either by editing this file,
//  or by leaving all the configuration block conditionals undefined in this file
//  and controlling them instead in the project options. The latter approach is
//  better if you want config-specific settings.
//  If controlling via this file: To turn a parameter ON, change or ensure the
//  respective directive from
//   {.$DEFINE XXX} to {$DEFINE XXX}
//  To turn the parameter OFF, change or ensure it is the other way around.
//
// PARAMETER DESCRIPTION TABLE:
// Conditional                   | Significance                                 |  Default and recommended value
// ..............................+..............................................+................................
//  OTL_CACHE_SLACKSPACE_OFFSETS | If undefined, the TGp4AlignedInt and similar | OFF (undefined).
//                               |  slack-space containers recalculate the slack|
//                               |  space offset on each value access. If set,  |
//                               |  the offset calculation is cached, which     |
//                               |  should give a slightly superior performance.|
//
// ACTUATION:

     {.$DEFINE OTL_CACHE_SLACKSPACE_OFFSETS}

// -----------------------------------------------------------------------------
// --------------- END OF CONFIGURATION BLOCK ----------------------------------
// -----------------------------------------------------------------------------

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24}
    {$DEFINE XE3_OR_ABOVE}
  {$IFEND}
{$ENDIF}
