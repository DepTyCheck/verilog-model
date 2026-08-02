-- Seed: 18093113812419723047,13592003931158285879

entity zeixsjf is
  port (anc : buffer bit; ozzsdpxph : buffer integer; mcimkt : in bit_vector(2 to 4); krujovakh : in time);
end zeixsjf;

architecture z of zeixsjf is
  
begin
  
end z;

library ieee;
use ieee.std_logic_1164.all;

entity aqvim is
  port (fhlngm : in integer_vector(2 downto 4); pfvoia : buffer real_vector(4 to 3); soogzh : inout std_logic_vector(2 downto 1));
end aqvim;

architecture cytin of aqvim is
  signal aevvktdlv : time;
  signal fsabcer : integer;
  signal s : bit;
  signal ywvl : time;
  signal lpf : bit_vector(2 to 4);
  signal nngtqurvn : integer;
  signal n : bit;
begin
  maqlqvqazx : entity work.zeixsjf
    port map (anc => n, ozzsdpxph => nngtqurvn, mcimkt => lpf, krujovakh => ywvl);
  bjuiy : entity work.zeixsjf
    port map (anc => s, ozzsdpxph => fsabcer, mcimkt => lpf, krujovakh => aevvktdlv);
  
  -- Single-driven assignments
  pfvoia <= pfvoia;
  aevvktdlv <= aevvktdlv;
  ywvl <= ywvl;
  lpf <= ('1', '1', '1');
  
  -- Multi-driven assignments
  soogzh <= soogzh;
  soogzh <= soogzh;
  soogzh <= "X0";
end cytin;



-- Seed after: 2127236261055631853,13592003931158285879
