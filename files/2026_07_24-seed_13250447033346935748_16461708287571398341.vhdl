-- Seed: 13250447033346935748,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity ctulyly is
  port (oqzkzhx : in integer; eueeyp : buffer std_logic_vector(3 downto 3); gnvlty : out std_logic_vector(1 to 1));
end ctulyly;

architecture piaqgtfn of ctulyly is
  
begin
  -- Multi-driven assignments
  gnvlty <= gnvlty;
  gnvlty <= gnvlty;
  gnvlty <= (others => 'L');
end piaqgtfn;

entity jl is
  port (cg : out time_vector(4 downto 4); q : in integer);
end jl;

library ieee;
use ieee.std_logic_1164.all;

architecture e of jl is
  signal u : std_logic_vector(3 downto 3);
  signal k : std_logic_vector(1 to 1);
  signal dvl : integer;
  signal dfgb : std_logic_vector(1 to 1);
begin
  valipihwf : entity work.ctulyly
    port map (oqzkzhx => q, eueeyp => dfgb, gnvlty => dfgb);
  ixkwfz : entity work.ctulyly
    port map (oqzkzhx => dvl, eueeyp => dfgb, gnvlty => dfgb);
  ltdv : entity work.ctulyly
    port map (oqzkzhx => q, eueeyp => dfgb, gnvlty => k);
  wroel : entity work.ctulyly
    port map (oqzkzhx => q, eueeyp => u, gnvlty => dfgb);
end e;



-- Seed after: 4989616838412015273,16461708287571398341
