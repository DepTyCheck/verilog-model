-- Seed: 7102004682687784912,8412319452373742525

entity mlnumpndfm is
  port (xta : out integer);
end mlnumpndfm;

architecture qbsycthz of mlnumpndfm is
  
begin
  -- Single-driven assignments
  xta <= 2#1#;
end qbsycthz;

library ieee;
use ieee.std_logic_1164.all;

entity kxxvuhc is
  port (sm : out std_logic; kfzchb : in std_logic_vector(2 to 2); uw : buffer std_logic_vector(4 to 3));
end kxxvuhc;

architecture mbvswvj of kxxvuhc is
  
begin
  -- Multi-driven assignments
  uw <= "";
  uw <= uw;
  sm <= sm;
end mbvswvj;

entity asd is
  port (xxppify : inout time);
end asd;

library ieee;
use ieee.std_logic_1164.all;

architecture vbksmai of asd is
  signal owhpnudq : std_logic_vector(4 to 3);
  signal mjfiq : std_logic_vector(2 to 2);
  signal modqmejyx : std_logic;
  signal t : integer;
  signal aagzqp : integer;
begin
  nz : entity work.mlnumpndfm
    port map (xta => aagzqp);
  dd : entity work.mlnumpndfm
    port map (xta => t);
  ni : entity work.kxxvuhc
    port map (sm => modqmejyx, kfzchb => mjfiq, uw => owhpnudq);
  
  -- Single-driven assignments
  xxppify <= 8#5# fs;
  
  -- Multi-driven assignments
  modqmejyx <= 'X';
end vbksmai;



-- Seed after: 4275279045242838779,8412319452373742525
