-- Seed: 2496232964677901987,13857275728440271305

entity uxucjsv is
  port (ks : buffer character; al : buffer real; hsr : out real);
end uxucjsv;

architecture yl of uxucjsv is
  
begin
  -- Single-driven assignments
  hsr <= al;
  al <= hsr;
  ks <= 'b';
end yl;

library ieee;
use ieee.std_logic_1164.all;

entity uciquy is
  port (tqoqz : out std_logic_vector(4 downto 0));
end uciquy;

architecture qx of uciquy is
  signal puarwc : real;
  signal eqxji : real;
  signal qsi : character;
  signal sqgbdo : real;
  signal d : real;
  signal rlhofk : character;
begin
  o : entity work.uxucjsv
    port map (ks => rlhofk, al => d, hsr => sqgbdo);
  youzupnih : entity work.uxucjsv
    port map (ks => qsi, al => eqxji, hsr => puarwc);
  
  -- Multi-driven assignments
  tqoqz <= tqoqz;
  tqoqz <= ('U', 'Z', '1', 'L', 'X');
  tqoqz <= tqoqz;
  tqoqz <= ('U', '1', 'U', 'U', '1');
end qx;

entity jgofidc is
  port (wy : inout severity_level);
end jgofidc;

architecture qzaxhcbma of jgofidc is
  signal aiouthpybl : real;
  signal lelyln : real;
  signal g : character;
  signal x : real;
  signal ftc : real;
  signal xl : character;
begin
  ambaymthg : entity work.uxucjsv
    port map (ks => xl, al => ftc, hsr => x);
  fzyh : entity work.uxucjsv
    port map (ks => g, al => lelyln, hsr => aiouthpybl);
  
  -- Single-driven assignments
  wy <= wy;
end qzaxhcbma;



-- Seed after: 845221113142231564,13857275728440271305
