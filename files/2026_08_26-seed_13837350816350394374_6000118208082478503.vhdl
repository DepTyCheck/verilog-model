-- Seed: 13837350816350394374,6000118208082478503

entity zsvtcke is
  port (mwg : in time; dcref : in real; zce : buffer integer);
end zsvtcke;

architecture smzdedu of zsvtcke is
  
begin
  -- Single-driven assignments
  zce <= zce;
end smzdedu;

entity tyj is
  port (f : in real; lbofdwqd : in boolean);
end tyj;

architecture kvoncsb of tyj is
  signal q : integer;
  signal extqsq : time;
begin
  z : entity work.zsvtcke
    port map (mwg => extqsq, dcref => f, zce => q);
  
  -- Single-driven assignments
  extqsq <= extqsq;
end kvoncsb;

entity rdq is
  port (yuzqhmavkz : inout boolean);
end rdq;

architecture hbwz of rdq is
  signal udr : integer;
  signal zxlqoak : integer;
  signal ym : real;
  signal s : time;
begin
  tez : entity work.zsvtcke
    port map (mwg => s, dcref => ym, zce => zxlqoak);
  fkxxp : entity work.zsvtcke
    port map (mwg => s, dcref => ym, zce => udr);
  
  -- Single-driven assignments
  yuzqhmavkz <= FALSE;
  s <= 8#437.4_3# ps;
  ym <= 2_1_1_2.21;
end hbwz;

library ieee;
use ieee.std_logic_1164.all;

entity bvjjg is
  port (uifhmi : out character; cd : linkage std_logic_vector(2 downto 3); p : out std_logic_vector(4 downto 4));
end bvjjg;

architecture qikc of bvjjg is
  signal hdxzpbi : integer;
  signal gtzyjbm : integer;
  signal ethfchlm : real;
  signal lmcicmp : time;
  signal s : integer;
  signal pqeyikurv : real;
  signal onz : time;
  signal jlgxqrsyh : integer;
  signal wcadzdxh : real;
  signal lw : time;
begin
  rjjc : entity work.zsvtcke
    port map (mwg => lw, dcref => wcadzdxh, zce => jlgxqrsyh);
  om : entity work.zsvtcke
    port map (mwg => onz, dcref => pqeyikurv, zce => s);
  rs : entity work.zsvtcke
    port map (mwg => lmcicmp, dcref => ethfchlm, zce => gtzyjbm);
  qen : entity work.zsvtcke
    port map (mwg => lmcicmp, dcref => wcadzdxh, zce => hdxzpbi);
  
  -- Multi-driven assignments
  p <= p;
  p <= p;
  p <= "X";
end qikc;



-- Seed after: 10487534609431683848,6000118208082478503
