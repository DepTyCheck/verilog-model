-- Seed: 15869931565212550492,10871023049702252113

entity wjh is
  port (jfdxv : in boolean; nmkwvq : out real);
end wjh;

architecture n of wjh is
  
begin
  
end n;

library ieee;
use ieee.std_logic_1164.all;

entity zt is
  port ( lnkfd : buffer std_logic_vector(0 downto 0)
  ; qsei : buffer real_vector(1 downto 0)
  ; akieo : out std_logic_vector(2 to 3)
  ; bjwwtly : inout integer
  );
end zt;

architecture dpdw of zt is
  
begin
  -- Single-driven assignments
  bjwwtly <= bjwwtly;
  qsei <= qsei;
  
  -- Multi-driven assignments
  akieo <= akieo;
end dpdw;

entity zfhobhduh is
  port (ib : inout boolean; rdfmhld : in integer_vector(0 downto 1); ketlwx : buffer boolean);
end zfhobhduh;

architecture lx of zfhobhduh is
  signal giilexpesq : real;
  signal ziscs : real;
  signal o : boolean;
  signal ufrsl : real;
  signal bpgtgdff : boolean;
  signal go : real;
  signal sdaorpo : boolean;
begin
  cclxglook : entity work.wjh
    port map (jfdxv => sdaorpo, nmkwvq => go);
  yszgxlojp : entity work.wjh
    port map (jfdxv => bpgtgdff, nmkwvq => ufrsl);
  h : entity work.wjh
    port map (jfdxv => o, nmkwvq => ziscs);
  t : entity work.wjh
    port map (jfdxv => ketlwx, nmkwvq => giilexpesq);
  
  -- Single-driven assignments
  ketlwx <= TRUE;
  ib <= FALSE;
  o <= o;
end lx;

library ieee;
use ieee.std_logic_1164.all;

entity pqg is
  port (utg : out std_logic_vector(1 downto 1); jhdlthfp : in integer; jlhqhb : out integer; yeso : out integer);
end pqg;

library ieee;
use ieee.std_logic_1164.all;

architecture bxiecyvho of pqg is
  signal cg : boolean;
  signal pbsts : integer_vector(0 downto 1);
  signal ya : boolean;
  signal qfuxmgyqqb : std_logic_vector(2 to 3);
  signal enzmnp : real_vector(1 downto 0);
  signal oj : real;
  signal azrse : boolean;
begin
  iowxniq : entity work.wjh
    port map (jfdxv => azrse, nmkwvq => oj);
  lzjgitzjw : entity work.zt
    port map (lnkfd => utg, qsei => enzmnp, akieo => qfuxmgyqqb, bjwwtly => jlhqhb);
  czkbzzybva : entity work.zfhobhduh
    port map (ib => ya, rdfmhld => pbsts, ketlwx => cg);
  
  -- Multi-driven assignments
  utg <= utg;
  utg <= (others => 'W');
  qfuxmgyqqb <= qfuxmgyqqb;
end bxiecyvho;



-- Seed after: 14025708441506225989,10871023049702252113
