-- Seed: 15888269588380110878,7304262412290825129

entity o is
  port (oc : out character; rwb : buffer integer; ex : buffer severity_level; nxddxhud : in real);
end o;

architecture mcdk of o is
  
begin
  -- Single-driven assignments
  oc <= 'd';
  rwb <= rwb;
  ex <= ex;
end mcdk;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (vgijnz : out integer; t : inout std_logic_vector(4 downto 4); i : out std_logic; yvlgngnio : linkage integer);
end f;

architecture nmwtsdlvs of f is
  signal znukquk : real;
  signal pawpgljm : severity_level;
  signal c : character;
  signal yz : real;
  signal opjqs : severity_level;
  signal zgs : integer;
  signal q : character;
  signal lomfh : real;
  signal fzbhs : severity_level;
  signal csjcpbs : integer;
  signal zfqlyugef : character;
  signal voybm : real;
  signal nr : severity_level;
  signal nrfastkrqf : integer;
  signal rmjoihnkdb : character;
begin
  aypkwr : entity work.o
    port map (oc => rmjoihnkdb, rwb => nrfastkrqf, ex => nr, nxddxhud => voybm);
  wvh : entity work.o
    port map (oc => zfqlyugef, rwb => csjcpbs, ex => fzbhs, nxddxhud => lomfh);
  tfcvgslyn : entity work.o
    port map (oc => q, rwb => zgs, ex => opjqs, nxddxhud => yz);
  fr : entity work.o
    port map (oc => c, rwb => vgijnz, ex => pawpgljm, nxddxhud => znukquk);
  
  -- Single-driven assignments
  lomfh <= voybm;
  
  -- Multi-driven assignments
  i <= 'U';
end nmwtsdlvs;



-- Seed after: 12520074799710428952,7304262412290825129
