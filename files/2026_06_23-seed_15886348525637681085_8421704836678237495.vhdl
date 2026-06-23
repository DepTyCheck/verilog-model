-- Seed: 15886348525637681085,8421704836678237495

entity gbtf is
  port (iea : in real_vector(3 to 1));
end gbtf;

architecture figrb of gbtf is
  
begin
  
end figrb;

library ieee;
use ieee.std_logic_1164.all;

entity qlplt is
  port (rziodzmsf : inout std_logic; fspnhhpyp : buffer integer);
end qlplt;

architecture qpcdhoiqv of qlplt is
  signal moqafqlsc : real_vector(3 to 1);
  signal qlcuvrqavp : real_vector(3 to 1);
begin
  dzmhy : entity work.gbtf
    port map (iea => qlcuvrqavp);
  pyjyrugszh : entity work.gbtf
    port map (iea => moqafqlsc);
  
  -- Single-driven assignments
  fspnhhpyp <= 1;
  moqafqlsc <= (others => 0.0);
  qlcuvrqavp <= (others => 0.0);
  
  -- Multi-driven assignments
  rziodzmsf <= 'L';
end qpcdhoiqv;

library ieee;
use ieee.std_logic_1164.all;

entity pgmjnkbbu is
  port (fk : in std_logic; gqnmssdrrq : inout real; r : inout integer; s : inout boolean_vector(1 downto 3));
end pgmjnkbbu;

library ieee;
use ieee.std_logic_1164.all;

architecture uamjovx of pgmjnkbbu is
  signal m : real_vector(3 to 1);
  signal z : integer;
  signal xssf : std_logic;
begin
  pfebre : entity work.qlplt
    port map (rziodzmsf => xssf, fspnhhpyp => z);
  kozqy : entity work.gbtf
    port map (iea => m);
  ezddyfxf : entity work.gbtf
    port map (iea => m);
  
  -- Single-driven assignments
  s <= (others => TRUE);
  r <= 2#01011#;
  
  -- Multi-driven assignments
  xssf <= 'Z';
  xssf <= 'W';
  xssf <= 'Z';
end uamjovx;



-- Seed after: 6693171400036982961,8421704836678237495
