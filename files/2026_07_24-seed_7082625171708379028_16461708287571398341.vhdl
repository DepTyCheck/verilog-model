-- Seed: 7082625171708379028,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity obnni is
  port (kcq : buffer std_logic; zmcrwgchz : buffer real_vector(4 downto 4); hbnwfevz : buffer integer);
end obnni;

architecture b of obnni is
  
begin
  -- Single-driven assignments
  hbnwfevz <= 2#01#;
  zmcrwgchz <= (others => 113.3);
  
  -- Multi-driven assignments
  kcq <= kcq;
  kcq <= kcq;
  kcq <= kcq;
end b;

library ieee;
use ieee.std_logic_1164.all;

entity qid is
  port (fldspgt : buffer std_logic_vector(0 to 4));
end qid;

library ieee;
use ieee.std_logic_1164.all;

architecture ofceuga of qid is
  signal ahoswhzsk : integer;
  signal hcuzkpyed : real_vector(4 downto 4);
  signal rgiqdtu : std_logic;
  signal mlcanway : integer;
  signal dh : real_vector(4 downto 4);
  signal wgmbhym : integer;
  signal wod : real_vector(4 downto 4);
  signal kyhej : std_logic;
  signal llobkqhzn : integer;
  signal oyz : real_vector(4 downto 4);
  signal pz : std_logic;
begin
  kurjjkch : entity work.obnni
    port map (kcq => pz, zmcrwgchz => oyz, hbnwfevz => llobkqhzn);
  etygbkyun : entity work.obnni
    port map (kcq => kyhej, zmcrwgchz => wod, hbnwfevz => wgmbhym);
  odrgt : entity work.obnni
    port map (kcq => kyhej, zmcrwgchz => dh, hbnwfevz => mlcanway);
  xwguai : entity work.obnni
    port map (kcq => rgiqdtu, zmcrwgchz => hcuzkpyed, hbnwfevz => ahoswhzsk);
  
  -- Multi-driven assignments
  fldspgt <= fldspgt;
  fldspgt <= fldspgt;
  kyhej <= pz;
end ofceuga;



-- Seed after: 7115202882652422407,16461708287571398341
