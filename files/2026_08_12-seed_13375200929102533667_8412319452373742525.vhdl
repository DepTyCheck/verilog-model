-- Seed: 13375200929102533667,8412319452373742525

entity cmq is
  port (nmefycx : in boolean_vector(3 to 1); ppghvneplv : in integer_vector(4 downto 4));
end cmq;

architecture tzhltugktm of cmq is
  
begin
  
end tzhltugktm;

entity yfktlzx is
  port (veefx : inout time);
end yfktlzx;

architecture r of yfktlzx is
  signal sorfpjbgsq : integer_vector(4 downto 4);
  signal pjs : boolean_vector(3 to 1);
begin
  iaxk : entity work.cmq
    port map (nmefycx => pjs, ppghvneplv => sorfpjbgsq);
  
  -- Single-driven assignments
  veefx <= 4_2_0.10 fs;
  pjs <= (others => TRUE);
  sorfpjbgsq <= (others => 16#5#);
end r;

library ieee;
use ieee.std_logic_1164.all;

entity pwxrc is
  port (zwtdcq : out std_logic);
end pwxrc;

architecture pbjtu of pwxrc is
  signal dsdntjhfu : time;
  signal td : integer_vector(4 downto 4);
  signal bgyjyynstf : boolean_vector(3 to 1);
  signal gz : time;
begin
  csagtns : entity work.yfktlzx
    port map (veefx => gz);
  rttvjf : entity work.cmq
    port map (nmefycx => bgyjyynstf, ppghvneplv => td);
  o : entity work.yfktlzx
    port map (veefx => dsdntjhfu);
  siczogdz : entity work.cmq
    port map (nmefycx => bgyjyynstf, ppghvneplv => td);
  
  -- Single-driven assignments
  td <= td;
  bgyjyynstf <= (others => TRUE);
  
  -- Multi-driven assignments
  zwtdcq <= zwtdcq;
  zwtdcq <= zwtdcq;
end pbjtu;



-- Seed after: 17527346051487655097,8412319452373742525
