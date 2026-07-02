-- Seed: 6018444431004403939,14426950258250697445

entity bruxdy is
  port (okao : in time; pczxn : out time; paxcci : inout time);
end bruxdy;

architecture d of bruxdy is
  
begin
  -- Single-driven assignments
  paxcci <= 0 hr;
  pczxn <= okao;
end d;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity jowd is
  port (wrdjccv : inout std_logic; ep : inout enumeration_subtype_mirror);
end jowd;

architecture zzqlkbvs of jowd is
  signal wbwmojz : time;
  signal ycwzujxt : time;
  signal yywjvqocyp : time;
  signal qquhk : time;
  signal qnedjgz : time;
  signal lcv : time;
  signal lnpwhyyfq : time;
  signal ncotrjy : time;
begin
  hwdjh : entity work.bruxdy
    port map (okao => ncotrjy, pczxn => lnpwhyyfq, paxcci => lcv);
  uh : entity work.bruxdy
    port map (okao => qnedjgz, pczxn => qnedjgz, paxcci => ncotrjy);
  tgdjbqafgk : entity work.bruxdy
    port map (okao => qquhk, pczxn => qquhk, paxcci => yywjvqocyp);
  uudcghd : entity work.bruxdy
    port map (okao => ncotrjy, pczxn => ycwzujxt, paxcci => wbwmojz);
  
  -- Multi-driven assignments
  wrdjccv <= wrdjccv;
  wrdjccv <= 'Z';
  wrdjccv <= '1';
  wrdjccv <= 'L';
end zzqlkbvs;



-- Seed after: 16655271748174136713,14426950258250697445
