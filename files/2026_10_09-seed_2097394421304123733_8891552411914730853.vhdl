-- Seed: 2097394421304123733,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity wpira is
  port (qmombdorx : buffer std_logic; s : buffer std_logic);
end wpira;

architecture ms of wpira is
  
begin
  -- Multi-driven assignments
  s <= qmombdorx;
end ms;

library ieee;
use ieee.std_logic_1164.all;

entity ltsfslxlyh is
  port (m : buffer std_logic);
end ltsfslxlyh;

library ieee;
use ieee.std_logic_1164.all;

architecture dzspyc of ltsfslxlyh is
  signal ptfstd : std_logic;
  signal gyvgrgwww : std_logic;
  signal d : std_logic;
  signal z : std_logic;
begin
  r : entity work.wpira
    port map (qmombdorx => z, s => d);
  kbiehp : entity work.wpira
    port map (qmombdorx => gyvgrgwww, s => ptfstd);
  
  -- Multi-driven assignments
  z <= ptfstd;
  m <= 'L';
end dzspyc;

library ieee;
use ieee.std_logic_1164.all;

entity amnludtjdc is
  port (iozxedzkc : inout real; du : linkage time; cga : inout severity_level; i : linkage std_logic_vector(1 to 0));
end amnludtjdc;

library ieee;
use ieee.std_logic_1164.all;

architecture rq of amnludtjdc is
  signal x : std_logic;
begin
  w : entity work.wpira
    port map (qmombdorx => x, s => x);
  
  -- Single-driven assignments
  cga <= cga;
  iozxedzkc <= iozxedzkc;
end rq;

library ieee;
use ieee.std_logic_1164.all;

entity fu is
  port (cfafjxlq : in std_logic_vector(1 downto 3); yah : buffer std_logic; tlxhhbi : buffer integer);
end fu;

architecture bzfzqaueum of fu is
  
begin
  dl : entity work.wpira
    port map (qmombdorx => yah, s => yah);
  
  -- Single-driven assignments
  tlxhhbi <= 2#010#;
  
  -- Multi-driven assignments
  yah <= yah;
  yah <= 'H';
  yah <= yah;
  yah <= yah;
end bzfzqaueum;



-- Seed after: 6371074634229368744,8891552411914730853
