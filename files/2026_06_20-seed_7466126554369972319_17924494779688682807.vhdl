-- Seed: 7466126554369972319,17924494779688682807

entity ckt is
  port (kn : inout boolean);
end ckt;

architecture ysnhg of ckt is
  
begin
  -- Single-driven assignments
  kn <= TRUE;
end ysnhg;

library ieee;
use ieee.std_logic_1164.all;

entity iw is
  port (cwvlahoc : buffer real; fhegw : in std_logic_vector(1 downto 1));
end iw;

architecture uh of iw is
  signal cfxnsb : boolean;
  signal bkur : boolean;
  signal qxknzhxkzf : boolean;
begin
  v : entity work.ckt
    port map (kn => qxknzhxkzf);
  uhwqjm : entity work.ckt
    port map (kn => bkur);
  jkpawhsysi : entity work.ckt
    port map (kn => cfxnsb);
  
  -- Single-driven assignments
  cwvlahoc <= 2#00.1#;
end uh;

entity lpilddvhv is
  port (sdpowtsb : buffer real);
end lpilddvhv;

library ieee;
use ieee.std_logic_1164.all;

architecture tj of lpilddvhv is
  signal adwypsh : boolean;
  signal lugppub : std_logic_vector(1 downto 1);
  signal jshnixs : real;
  signal dvsj : boolean;
  signal ikun : boolean;
begin
  mytl : entity work.ckt
    port map (kn => ikun);
  pkck : entity work.ckt
    port map (kn => dvsj);
  qiuet : entity work.iw
    port map (cwvlahoc => jshnixs, fhegw => lugppub);
  s : entity work.ckt
    port map (kn => adwypsh);
  
  -- Single-driven assignments
  sdpowtsb <= 16#F_1_7_7.F_F#;
  
  -- Multi-driven assignments
  lugppub <= (others => '1');
end tj;

entity ikw is
  port (cbpjxy : linkage integer);
end ikw;

architecture wbybnutk of ikw is
  signal zbnjedqowp : boolean;
  signal zb : real;
begin
  gcqdjnjuo : entity work.lpilddvhv
    port map (sdpowtsb => zb);
  emr : entity work.ckt
    port map (kn => zbnjedqowp);
end wbybnutk;



-- Seed after: 7382662547238497765,17924494779688682807
