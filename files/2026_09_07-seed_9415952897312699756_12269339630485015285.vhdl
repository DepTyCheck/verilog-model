-- Seed: 9415952897312699756,12269339630485015285

entity hpgcsqaqm is
  port (fd : out severity_level; e : inout real; po : inout time_vector(3 downto 4); jcqsk : inout integer);
end hpgcsqaqm;

architecture tuwtwzhlz of hpgcsqaqm is
  
begin
  -- Single-driven assignments
  e <= 16#D.E_4_1#;
  po <= po;
  jcqsk <= jcqsk;
end tuwtwzhlz;

library ieee;
use ieee.std_logic_1164.all;

entity trmeq is
  port (vxjt : in boolean_vector(3 downto 4); tsinf : inout std_logic_vector(3 to 0); qyi : in integer; sdjp : in integer);
end trmeq;

architecture nb of trmeq is
  signal mslpfljx : integer;
  signal m : time_vector(3 downto 4);
  signal bg : real;
  signal jqef : severity_level;
  signal mqtdobgked : integer;
  signal dtwvisczm : time_vector(3 downto 4);
  signal uaxx : real;
  signal lmmyehpu : severity_level;
  signal ijb : integer;
  signal pf : time_vector(3 downto 4);
  signal twdki : real;
  signal hr : severity_level;
begin
  qdmqn : entity work.hpgcsqaqm
    port map (fd => hr, e => twdki, po => pf, jcqsk => ijb);
  ekvb : entity work.hpgcsqaqm
    port map (fd => lmmyehpu, e => uaxx, po => dtwvisczm, jcqsk => mqtdobgked);
  wr : entity work.hpgcsqaqm
    port map (fd => jqef, e => bg, po => m, jcqsk => mslpfljx);
  
  -- Multi-driven assignments
  tsinf <= "";
end nb;



-- Seed after: 1630416158145206485,12269339630485015285
