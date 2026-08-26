-- Seed: 10579472964438877550,6000118208082478503

entity z is
  port (uvzl : in time_vector(3 downto 1); hwtf : in time; vnqopo : out severity_level; bwads : out string(3 to 4));
end z;

architecture rvtzno of z is
  
begin
  -- Single-driven assignments
  bwads <= "ec";
  vnqopo <= ERROR;
end rvtzno;

entity ahsb is
  port (cxpmhoj : inout character);
end ahsb;

architecture khq of ahsb is
  signal fyjji : string(3 to 4);
  signal prmd : severity_level;
  signal zoft : time;
  signal zw : string(3 to 4);
  signal gg : severity_level;
  signal zypffzk : time;
  signal bnxixcrbvk : time_vector(3 downto 1);
begin
  gmddvzhdu : entity work.z
    port map (uvzl => bnxixcrbvk, hwtf => zypffzk, vnqopo => gg, bwads => zw);
  hmkaae : entity work.z
    port map (uvzl => bnxixcrbvk, hwtf => zoft, vnqopo => prmd, bwads => fyjji);
end khq;

library ieee;
use ieee.std_logic_1164.all;

entity bjewr is
  port (ysnjijpnco : out std_logic_vector(4 to 4); mivwzxlv : buffer time; rpzoiimjdu : inout integer);
end bjewr;

architecture tvwv of bjewr is
  signal x : string(3 to 4);
  signal et : severity_level;
  signal ufsfh : string(3 to 4);
  signal lvxfk : severity_level;
  signal sdwhckir : time_vector(3 downto 1);
  signal atcf : character;
  signal jld : string(3 to 4);
  signal fhpepfsyhz : severity_level;
  signal blirgyll : time_vector(3 downto 1);
begin
  b : entity work.z
    port map (uvzl => blirgyll, hwtf => mivwzxlv, vnqopo => fhpepfsyhz, bwads => jld);
  aymhy : entity work.ahsb
    port map (cxpmhoj => atcf);
  cugicvflma : entity work.z
    port map (uvzl => sdwhckir, hwtf => mivwzxlv, vnqopo => lvxfk, bwads => ufsfh);
  fauvasn : entity work.z
    port map (uvzl => blirgyll, hwtf => mivwzxlv, vnqopo => et, bwads => x);
  
  -- Single-driven assignments
  rpzoiimjdu <= rpzoiimjdu;
  blirgyll <= blirgyll;
  mivwzxlv <= 3223.04 ps;
  
  -- Multi-driven assignments
  ysnjijpnco <= (others => 'X');
  ysnjijpnco <= ysnjijpnco;
end tvwv;



-- Seed after: 9538064248168292471,6000118208082478503
