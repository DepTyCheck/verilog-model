-- Seed: 16691983859917712221,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity xydp is
  port (mw : in bit_vector(3 to 3); vyafed : linkage integer; pdtxmkxzsk : buffer bit_vector(1 to 1); soxr : inout std_logic_vector(1 downto 2));
end xydp;

architecture xfpr of xydp is
  
begin
  -- Single-driven assignments
  pdtxmkxzsk <= pdtxmkxzsk;
  
  -- Multi-driven assignments
  soxr <= (others => '0');
end xfpr;

entity tezofc is
  port (zi : in time);
end tezofc;

library ieee;
use ieee.std_logic_1164.all;

architecture k of tezofc is
  signal kwotkdjxma : std_logic_vector(1 downto 2);
  signal yveno : bit_vector(1 to 1);
  signal d : integer;
  signal n : bit_vector(1 to 1);
  signal bxvuogt : integer;
  signal psbd : bit_vector(3 to 3);
  signal h : std_logic_vector(1 downto 2);
  signal pysi : bit_vector(3 to 3);
  signal crzswv : integer;
  signal wsyw : bit_vector(3 to 3);
begin
  u : entity work.xydp
    port map (mw => wsyw, vyafed => crzswv, pdtxmkxzsk => pysi, soxr => h);
  qel : entity work.xydp
    port map (mw => psbd, vyafed => bxvuogt, pdtxmkxzsk => n, soxr => h);
  q : entity work.xydp
    port map (mw => pysi, vyafed => d, pdtxmkxzsk => yveno, soxr => kwotkdjxma);
  
  -- Single-driven assignments
  psbd <= wsyw;
  wsyw <= pysi;
  
  -- Multi-driven assignments
  h <= h;
end k;



-- Seed after: 6346531399674292513,13857275728440271305
