-- Seed: 12964042550400278806,3181554006726329157

use std.reflection.all;

entity nvr is
  port (bsnc : inout enumeration_subtype_mirror);
end nvr;

architecture xscvguna of nvr is
  
begin
  
end xscvguna;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity bzrj is
  port (tixxf : in time; d : buffer std_logic_vector(4 to 2); esjfd : linkage std_logic; ymg : inout physical_subtype_mirror);
end bzrj;

architecture fm of bzrj is
  
begin
  
end fm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity yfihkx is
  port (crfoeukbkn : inout subtype_mirror; c : out std_logic_vector(4 to 2));
end yfihkx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture yscuo of yfihkx is
  shared variable i : enumeration_subtype_mirror;
  shared variable nzbb : physical_subtype_mirror;
  signal wowueouqw : std_logic;
  signal svlqvzezrx : std_logic_vector(4 to 2);
  signal n : time;
  shared variable njwgbb : enumeration_subtype_mirror;
  shared variable k : enumeration_subtype_mirror;
begin
  kgvo : entity work.nvr
    port map (bsnc => k);
  srxupq : entity work.nvr
    port map (bsnc => njwgbb);
  eqrmy : entity work.bzrj
    port map (tixxf => n, d => svlqvzezrx, esjfd => wowueouqw, ymg => nzbb);
  cuhguhw : entity work.nvr
    port map (bsnc => i);
  
  -- Single-driven assignments
  n <= n;
  
  -- Multi-driven assignments
  c <= (others => '0');
end yscuo;

use std.reflection.all;

entity pmyumrl is
  port (ufgsfjolp : in severity_level; yykd : in integer; aijuocmtx : inout floating_subtype_mirror);
end pmyumrl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture un of pmyumrl is
  shared variable zejpsgwk : physical_subtype_mirror;
  signal xwtehsi : std_logic;
  signal xnjftpdru : std_logic_vector(4 to 2);
  signal vl : time;
  shared variable zhrppo : enumeration_subtype_mirror;
  shared variable mbdgfr : enumeration_subtype_mirror;
begin
  z : entity work.nvr
    port map (bsnc => mbdgfr);
  tmubghhfmr : entity work.nvr
    port map (bsnc => zhrppo);
  eyoblhhkdy : entity work.bzrj
    port map (tixxf => vl, d => xnjftpdru, esjfd => xwtehsi, ymg => zejpsgwk);
  
  -- Single-driven assignments
  vl <= 041.01322 us;
  
  -- Multi-driven assignments
  xwtehsi <= '1';
  xnjftpdru <= (others => '0');
  xnjftpdru <= (others => '0');
end un;



-- Seed after: 12039482822062846410,3181554006726329157
