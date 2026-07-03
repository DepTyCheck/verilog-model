-- Seed: 3999495804404319651,2158184632809654795

use std.reflection.all;

entity vwwzjvy is
  port (ruyexqu : inout file_value_mirror; b : out time_vector(3 downto 2); r : out character; ve : in time);
end vwwzjvy;

architecture ovxbaf of vwwzjvy is
  
begin
  
end ovxbaf;

use std.reflection.all;

entity s is
  port (zjd : linkage time; ljadrqqkj : inout floating_value_mirror; pprcvrs : linkage integer; rwonl : inout protected_subtype_mirror);
end s;

use std.reflection.all;

architecture lwbyh of s is
  signal yxsmztps : character;
  signal zqvgs : time_vector(3 downto 2);
  shared variable uwptvrjp : file_value_mirror;
  signal r : time;
  signal kiavvikh : character;
  signal hl : time_vector(3 downto 2);
  shared variable xbkubskl : file_value_mirror;
  signal tupptnm : time;
  signal xw : character;
  signal neu : time_vector(3 downto 2);
  shared variable z : file_value_mirror;
begin
  fow : entity work.vwwzjvy
    port map (ruyexqu => z, b => neu, r => xw, ve => tupptnm);
  zmqmzfqx : entity work.vwwzjvy
    port map (ruyexqu => xbkubskl, b => hl, r => kiavvikh, ve => r);
  fc : entity work.vwwzjvy
    port map (ruyexqu => uwptvrjp, b => zqvgs, r => yxsmztps, ve => tupptnm);
  
  -- Single-driven assignments
  tupptnm <= 8#23# fs;
end lwbyh;

entity lkzfbvl is
  port (ranfuxsvpn : linkage real);
end lkzfbvl;

use std.reflection.all;

architecture kqkifxj of lkzfbvl is
  shared variable yewx : protected_subtype_mirror;
  signal h : integer;
  shared variable wekxvcp : floating_value_mirror;
  signal u : time;
begin
  csqhho : entity work.s
    port map (zjd => u, ljadrqqkj => wekxvcp, pprcvrs => h, rwonl => yewx);
end kqkifxj;



-- Seed after: 8592360093834114671,2158184632809654795
