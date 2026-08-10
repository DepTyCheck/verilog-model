-- Seed: 2462701668453849246,2338584220606314193

entity fdhpl is
  port (cofsa : buffer time; yrcxr : buffer boolean_vector(2 downto 4));
end fdhpl;

architecture v of fdhpl is
  
begin
  -- Single-driven assignments
  yrcxr <= yrcxr;
  cofsa <= 0_3_0_1 ms;
end v;

library ieee;
use ieee.std_logic_1164.all;

entity maz is
  port (axkfmpxgsb : inout std_logic);
end maz;

architecture k of maz is
  signal vmqrpcrakn : boolean_vector(2 downto 4);
  signal fh : time;
  signal spixfrf : boolean_vector(2 downto 4);
  signal c : time;
  signal oecmsdch : boolean_vector(2 downto 4);
  signal rpqk : time;
begin
  arqyvg : entity work.fdhpl
    port map (cofsa => rpqk, yrcxr => oecmsdch);
  vbblxczgw : entity work.fdhpl
    port map (cofsa => c, yrcxr => spixfrf);
  rgxhjks : entity work.fdhpl
    port map (cofsa => fh, yrcxr => vmqrpcrakn);
  
  -- Multi-driven assignments
  axkfmpxgsb <= '-';
end k;

entity xetmeopboj is
  port (mxxxie : in real; xgeiojjb : buffer bit; bplgajwq : linkage real);
end xetmeopboj;

architecture qesmfi of xetmeopboj is
  signal ozhalcoqtv : boolean_vector(2 downto 4);
  signal lqwgloruq : time;
begin
  cdjciaegt : entity work.fdhpl
    port map (cofsa => lqwgloruq, yrcxr => ozhalcoqtv);
  
  -- Single-driven assignments
  xgeiojjb <= xgeiojjb;
end qesmfi;

library ieee;
use ieee.std_logic_1164.all;

entity ukojodbso is
  port (od : inout boolean_vector(2 downto 3); cztqpn : linkage std_logic_vector(2 downto 3));
end ukojodbso;

library ieee;
use ieee.std_logic_1164.all;

architecture oiouij of ukojodbso is
  signal vb : std_logic;
  signal u : time;
begin
  vuueq : entity work.fdhpl
    port map (cofsa => u, yrcxr => od);
  wsrt : entity work.maz
    port map (axkfmpxgsb => vb);
  
  -- Multi-driven assignments
  vb <= vb;
  vb <= 'X';
  vb <= vb;
end oiouij;



-- Seed after: 8643542386785772650,2338584220606314193
