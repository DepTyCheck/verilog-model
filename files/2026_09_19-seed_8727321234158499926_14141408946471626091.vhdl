-- Seed: 8727321234158499926,14141408946471626091

entity qtr is
  port (efhz : linkage real; lzursb : buffer time);
end qtr;

architecture et of qtr is
  
begin
  -- Single-driven assignments
  lzursb <= lzursb;
end et;

library ieee;
use ieee.std_logic_1164.all;

entity tqmtai is
  port (mblbmzq : inout std_logic_vector(1 downto 0); xecu : in character; xr : buffer real);
end tqmtai;

architecture qgdov of tqmtai is
  signal gifnopnypl : time;
  signal zrnpoepvkd : time;
  signal gzp : real;
  signal zqlysf : time;
  signal cke : real;
begin
  xub : entity work.qtr
    port map (efhz => cke, lzursb => zqlysf);
  ptieprflqd : entity work.qtr
    port map (efhz => gzp, lzursb => zrnpoepvkd);
  qkerm : entity work.qtr
    port map (efhz => xr, lzursb => gifnopnypl);
  
  -- Multi-driven assignments
  mblbmzq <= mblbmzq;
end qgdov;



-- Seed after: 9222601425571510643,14141408946471626091
