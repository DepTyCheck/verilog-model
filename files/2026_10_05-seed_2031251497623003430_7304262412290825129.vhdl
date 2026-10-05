-- Seed: 2031251497623003430,7304262412290825129

entity njfp is
  port (dpuuj : in bit);
end njfp;

architecture xzouikiji of njfp is
  
begin
  
end xzouikiji;

entity o is
  port (xrixko : inout time);
end o;

architecture vefv of o is
  signal cqby : bit;
  signal ofehl : bit;
  signal dfky : bit;
  signal nkicj : bit;
begin
  pvp : entity work.njfp
    port map (dpuuj => nkicj);
  rrigp : entity work.njfp
    port map (dpuuj => dfky);
  adbqvpcxmn : entity work.njfp
    port map (dpuuj => ofehl);
  obmj : entity work.njfp
    port map (dpuuj => cqby);
end vefv;

library ieee;
use ieee.std_logic_1164.all;

entity qrtw is
  port (j : linkage real; mzdbydgu : linkage real; ayzgwcw : buffer std_logic_vector(1 downto 1); xtavy : inout std_logic_vector(4 downto 4));
end qrtw;

architecture kncegs of qrtw is
  signal c : bit;
  signal dfkpdl : bit;
begin
  dcuweefo : entity work.njfp
    port map (dpuuj => dfkpdl);
  pog : entity work.njfp
    port map (dpuuj => c);
  
  -- Single-driven assignments
  dfkpdl <= c;
  c <= '0';
  
  -- Multi-driven assignments
  xtavy <= (others => '1');
  ayzgwcw <= xtavy;
  xtavy <= xtavy;
end kncegs;



-- Seed after: 11676950708601937684,7304262412290825129
