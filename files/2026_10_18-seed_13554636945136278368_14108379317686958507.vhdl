-- Seed: 13554636945136278368,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity ckgbcraem is
  port (zkpoapvl : out real; an : out integer; uzi : inout real; kzdcc : linkage std_logic_vector(1 downto 1));
end ckgbcraem;

architecture ius of ckgbcraem is
  
begin
  -- Single-driven assignments
  an <= 16#7_0#;
  uzi <= zkpoapvl;
end ius;

entity wn is
  port (wpzj : buffer bit; vgktx : in bit_vector(3 downto 2); gqheh : inout character);
end wn;

library ieee;
use ieee.std_logic_1164.all;

architecture ntklxiwfpe of wn is
  signal j : real;
  signal m : integer;
  signal rzg : real;
  signal incvoan : std_logic_vector(1 downto 1);
  signal kgadlquour : real;
  signal edpchsz : integer;
  signal v : real;
begin
  cjtnr : entity work.ckgbcraem
    port map (zkpoapvl => v, an => edpchsz, uzi => kgadlquour, kzdcc => incvoan);
  ytwkkzaq : entity work.ckgbcraem
    port map (zkpoapvl => rzg, an => m, uzi => j, kzdcc => incvoan);
  
  -- Single-driven assignments
  gqheh <= 'l';
  wpzj <= '1';
  
  -- Multi-driven assignments
  incvoan <= incvoan;
  incvoan <= incvoan;
  incvoan <= "W";
  incvoan <= (others => 'X');
end ntklxiwfpe;



-- Seed after: 10048484800585592083,14108379317686958507
