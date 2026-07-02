-- Seed: 10980492248437401811,14426950258250697445

use std.reflection.all;

entity mvgsv is
  port (dztaawnl : inout enumeration_subtype_mirror; zfjrihncdw : inout access_value_mirror; j : buffer real);
end mvgsv;

architecture bbhhr of mvgsv is
  
begin
  -- Single-driven assignments
  j <= 2#0_1_1.0_0_0#;
end bbhhr;

use std.reflection.all;

entity luhy is
  port (cpdywx : inout enumeration_subtype_mirror; lam : inout value_mirror);
end luhy;

use std.reflection.all;

architecture ybo of luhy is
  signal kyzzzo : real;
  shared variable jtvulyxw : access_value_mirror;
  shared variable k : enumeration_subtype_mirror;
begin
  tdmmmbjzbz : entity work.mvgsv
    port map (dztaawnl => k, zfjrihncdw => jtvulyxw, j => kyzzzo);
end ybo;

use std.reflection.all;

entity zelgw is
  port (xabnwr : inout protected_value_mirror; qkrdj : out integer; eiql : inout record_value_mirror);
end zelgw;

use std.reflection.all;

architecture ec of zelgw is
  shared variable fgwbofucmd : value_mirror;
  shared variable jptivvdll : enumeration_subtype_mirror;
  signal kfhiukne : real;
  shared variable svgs : access_value_mirror;
  shared variable md : enumeration_subtype_mirror;
  signal zhtbnad : real;
  shared variable ycri : access_value_mirror;
  shared variable mbdofoxlv : enumeration_subtype_mirror;
  signal xq : real;
  shared variable kxlvh : access_value_mirror;
  shared variable npgyzvd : enumeration_subtype_mirror;
begin
  frtu : entity work.mvgsv
    port map (dztaawnl => npgyzvd, zfjrihncdw => kxlvh, j => xq);
  izu : entity work.mvgsv
    port map (dztaawnl => mbdofoxlv, zfjrihncdw => ycri, j => zhtbnad);
  w : entity work.mvgsv
    port map (dztaawnl => md, zfjrihncdw => svgs, j => kfhiukne);
  yguusi : entity work.luhy
    port map (cpdywx => jptivvdll, lam => fgwbofucmd);
  
  -- Single-driven assignments
  qkrdj <= 1044;
end ec;



-- Seed after: 5037444947982640293,14426950258250697445
