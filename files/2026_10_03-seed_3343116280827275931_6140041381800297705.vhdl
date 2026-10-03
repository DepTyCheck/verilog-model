-- Seed: 3343116280827275931,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity drpagt is
  port (xkp : buffer std_logic; r : linkage time_vector(1 downto 1); c : inout time_vector(4 downto 3));
end drpagt;

architecture idp of drpagt is
  
begin
  -- Single-driven assignments
  c <= (1 hr, 341.10024 fs);
  
  -- Multi-driven assignments
  xkp <= '1';
  xkp <= 'W';
  xkp <= '-';
  xkp <= xkp;
end idp;

entity thg is
  port (itmvxtjzeq : in character; ppcpxoj : buffer string(3 to 4));
end thg;

library ieee;
use ieee.std_logic_1164.all;

architecture huv of thg is
  signal wkpmqswkhm : time_vector(4 downto 3);
  signal gpsbmboeem : time_vector(1 downto 1);
  signal ijywadowhw : time_vector(4 downto 3);
  signal nrbz : time_vector(1 downto 1);
  signal lqbr : std_logic;
begin
  hen : entity work.drpagt
    port map (xkp => lqbr, r => nrbz, c => ijywadowhw);
  rk : entity work.drpagt
    port map (xkp => lqbr, r => gpsbmboeem, c => wkpmqswkhm);
  
  -- Single-driven assignments
  ppcpxoj <= "mb";
  
  -- Multi-driven assignments
  lqbr <= 'H';
  lqbr <= 'X';
end huv;



-- Seed after: 1563545676502234928,6140041381800297705
