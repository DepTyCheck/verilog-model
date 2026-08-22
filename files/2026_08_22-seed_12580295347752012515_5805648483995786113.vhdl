-- Seed: 12580295347752012515,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity eatvgaerd is
  port (ywdkfx : out severity_level; ibaclmggk : in std_logic);
end eatvgaerd;

architecture cij of eatvgaerd is
  
begin
  -- Single-driven assignments
  ywdkfx <= FAILURE;
end cij;

entity gblptragzw is
  port (kgzmimsfc : buffer time);
end gblptragzw;

library ieee;
use ieee.std_logic_1164.all;

architecture auqsvp of gblptragzw is
  signal znruu : severity_level;
  signal vinyj : std_logic;
  signal jkpqxhedk : severity_level;
begin
  zpgcw : entity work.eatvgaerd
    port map (ywdkfx => jkpqxhedk, ibaclmggk => vinyj);
  vj : entity work.eatvgaerd
    port map (ywdkfx => znruu, ibaclmggk => vinyj);
  
  -- Single-driven assignments
  kgzmimsfc <= 2 sec;
  
  -- Multi-driven assignments
  vinyj <= vinyj;
  vinyj <= 'U';
end auqsvp;



-- Seed after: 13710506955493911319,5805648483995786113
