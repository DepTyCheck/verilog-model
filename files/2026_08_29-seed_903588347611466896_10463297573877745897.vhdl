-- Seed: 903588347611466896,10463297573877745897

entity lsp is
  port (adazmfur : in integer; xzweqzhmr : inout string(2 to 5); keao : out boolean_vector(2 downto 1); jmlofrdiib : buffer time);
end lsp;

architecture eyegfheei of lsp is
  
begin
  -- Single-driven assignments
  keao <= (FALSE, TRUE);
  jmlofrdiib <= jmlofrdiib;
  xzweqzhmr <= xzweqzhmr;
end eyegfheei;

library ieee;
use ieee.std_logic_1164.all;

entity lcofv is
  port (zmlaekbmc : in integer; lypirq : out std_logic_vector(3 to 3));
end lcofv;

architecture bd of lcofv is
  signal wqi : time;
  signal zdfd : boolean_vector(2 downto 1);
  signal ldlkfwqztx : string(2 to 5);
  signal kkmqasly : integer;
  signal hci : time;
  signal p : boolean_vector(2 downto 1);
  signal lwopyezv : string(2 to 5);
  signal cnnmmaeue : time;
  signal oy : boolean_vector(2 downto 1);
  signal grenkj : string(2 to 5);
  signal mctxx : integer;
begin
  wvgoajcse : entity work.lsp
    port map (adazmfur => mctxx, xzweqzhmr => grenkj, keao => oy, jmlofrdiib => cnnmmaeue);
  s : entity work.lsp
    port map (adazmfur => mctxx, xzweqzhmr => lwopyezv, keao => p, jmlofrdiib => hci);
  bbzgh : entity work.lsp
    port map (adazmfur => kkmqasly, xzweqzhmr => ldlkfwqztx, keao => zdfd, jmlofrdiib => wqi);
  
  -- Single-driven assignments
  mctxx <= zmlaekbmc;
  kkmqasly <= 16#7_3#;
  
  -- Multi-driven assignments
  lypirq <= (others => '0');
  lypirq <= lypirq;
end bd;



-- Seed after: 858247511816627056,10463297573877745897
