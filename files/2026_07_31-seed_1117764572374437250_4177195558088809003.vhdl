-- Seed: 1117764572374437250,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity isj is
  port (tetrx : inout std_logic_vector(0 downto 2); qccg : linkage bit_vector(3 to 1); aazxuiddq : linkage integer; zwjovzb : inout time);
end isj;

architecture m of isj is
  
begin
  -- Multi-driven assignments
  tetrx <= (others => '0');
  tetrx <= tetrx;
  tetrx <= tetrx;
  tetrx <= (others => '0');
end m;

library ieee;
use ieee.std_logic_1164.all;

entity hwsi is
  port (mnrkr : in integer_vector(4 downto 2); rtiwd : in time_vector(3 to 2); szaaqf : in std_logic_vector(4 downto 1); ethh : out real);
end hwsi;

architecture kggru of hwsi is
  
begin
  
end kggru;

entity mnkacf is
  port (yctwevoafs : buffer real; lw : inout boolean);
end mnkacf;

library ieee;
use ieee.std_logic_1164.all;

architecture d of mnkacf is
  signal dzwi : time;
  signal prsqzggqq : integer;
  signal nats : bit_vector(3 to 1);
  signal tlmx : time;
  signal lrmakq : integer;
  signal nlhwvfu : bit_vector(3 to 1);
  signal xkvu : std_logic_vector(0 downto 2);
begin
  sjwr : entity work.isj
    port map (tetrx => xkvu, qccg => nlhwvfu, aazxuiddq => lrmakq, zwjovzb => tlmx);
  cak : entity work.isj
    port map (tetrx => xkvu, qccg => nats, aazxuiddq => prsqzggqq, zwjovzb => dzwi);
  
  -- Single-driven assignments
  lw <= TRUE;
  yctwevoafs <= 43030.2124;
  
  -- Multi-driven assignments
  xkvu <= "";
  xkvu <= "";
  xkvu <= xkvu;
  xkvu <= "";
end d;



-- Seed after: 11387728059226920846,4177195558088809003
