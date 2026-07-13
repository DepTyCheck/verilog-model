-- Seed: 4676368024983556220,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (nv : in std_logic);
end p;

architecture jfpi of p is
  
begin
  
end jfpi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity gsbvtb is
  port (voqekgh : inout std_logic; iqacxkt : buffer character; bvw : inout protected_subtype_mirror);
end gsbvtb;

architecture teeedr of gsbvtb is
  
begin
  ydffonar : entity work.p
    port map (nv => voqekgh);
  
  -- Single-driven assignments
  iqacxkt <= iqacxkt;
  
  -- Multi-driven assignments
  voqekgh <= '-';
  voqekgh <= voqekgh;
  voqekgh <= 'X';
end teeedr;

library ieee;
use ieee.std_logic_1164.all;

entity pgu is
  port (eaxpdvwy : in std_logic_vector(3 to 3); j : out boolean; sgregdfms : out boolean_vector(2 downto 3));
end pgu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture tyoeqaa of pgu is
  shared variable jjohkcwdlu : protected_subtype_mirror;
  signal hjh : character;
  signal xlarpy : std_logic;
  shared variable uibfj : protected_subtype_mirror;
  signal jijb : character;
  signal slc : std_logic;
begin
  gf : entity work.gsbvtb
    port map (voqekgh => slc, iqacxkt => jijb, bvw => uibfj);
  kpalo : entity work.p
    port map (nv => xlarpy);
  uvez : entity work.gsbvtb
    port map (voqekgh => slc, iqacxkt => hjh, bvw => jjohkcwdlu);
end tyoeqaa;

library ieee;
use ieee.std_logic_1164.all;

entity zkhxgu is
  port (zs : linkage severity_level; phyrworqe : out time; nwhwv : inout std_logic);
end zkhxgu;

library ieee;
use ieee.std_logic_1164.all;

architecture guhfkd of zkhxgu is
  signal bvvjr : std_logic;
  signal kciziybort : boolean_vector(2 downto 3);
  signal ifustg : boolean;
  signal jfmiry : std_logic_vector(3 to 3);
begin
  exnphc : entity work.p
    port map (nv => nwhwv);
  t : entity work.pgu
    port map (eaxpdvwy => jfmiry, j => ifustg, sgregdfms => kciziybort);
  xbflekxg : entity work.p
    port map (nv => bvvjr);
  
  -- Single-driven assignments
  phyrworqe <= 4_4_0.002 fs;
end guhfkd;



-- Seed after: 5389425921284661835,3566912872917928779
