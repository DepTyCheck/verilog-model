-- Seed: 4211723417765768382,4245627776430562977

entity yepvip is
  port (yoinp : in boolean; wks : linkage real_vector(3 to 4); zd : in real);
end yepvip;

architecture xwznuwnouk of yepvip is
  
begin
  
end xwznuwnouk;

entity wf is
  port (bzoirazh : linkage real; qtt : in string(5 to 3));
end wf;

architecture nppg of wf is
  signal pbuizpqbb : real_vector(3 to 4);
  signal dzfpvyd : real_vector(3 to 4);
  signal criooxi : boolean;
  signal rhryzqwwbc : real_vector(3 to 4);
  signal uxmpa : boolean;
  signal fpxbqffvd : real;
  signal hmuqpihkdg : real_vector(3 to 4);
  signal btwwvcgyz : boolean;
begin
  wsdqc : entity work.yepvip
    port map (yoinp => btwwvcgyz, wks => hmuqpihkdg, zd => fpxbqffvd);
  muh : entity work.yepvip
    port map (yoinp => uxmpa, wks => rhryzqwwbc, zd => fpxbqffvd);
  xqd : entity work.yepvip
    port map (yoinp => criooxi, wks => dzfpvyd, zd => fpxbqffvd);
  yilzpovyl : entity work.yepvip
    port map (yoinp => btwwvcgyz, wks => pbuizpqbb, zd => fpxbqffvd);
  
  -- Single-driven assignments
  btwwvcgyz <= TRUE;
  criooxi <= btwwvcgyz;
  uxmpa <= btwwvcgyz;
  fpxbqffvd <= fpxbqffvd;
end nppg;

library ieee;
use ieee.std_logic_1164.all;

entity lxqrtkfb is
  port (bpjaehojb : linkage std_logic_vector(4 to 1); cltr : out character);
end lxqrtkfb;

architecture oifzuqmgai of lxqrtkfb is
  signal pqrfkv : real;
  signal fty : real_vector(3 to 4);
  signal pfqdpo : boolean;
begin
  ebxac : entity work.yepvip
    port map (yoinp => pfqdpo, wks => fty, zd => pqrfkv);
  
  -- Single-driven assignments
  cltr <= 'g';
  pqrfkv <= 2#1100.11110#;
  pfqdpo <= pfqdpo;
end oifzuqmgai;



-- Seed after: 14395329663869972964,4245627776430562977
