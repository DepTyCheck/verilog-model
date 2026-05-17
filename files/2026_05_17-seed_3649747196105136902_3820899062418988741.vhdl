-- Seed: 3649747196105136902,3820899062418988741



entity ctzgzeh is
  port (r : linkage time; nktl : inout character; huy : in integer; tgaca : inout integer);
end ctzgzeh;



architecture vletwfyesz of ctzgzeh is
  
begin
  
end vletwfyesz;



entity pjl is
  port (pwscemfrvc : out time; xz : in time);
end pjl;



architecture zrg of pjl is
  signal x : integer;
  signal wcmnvlqzmm : character;
  signal qcmjc : time;
  signal moezbukhpg : integer;
  signal ysdz : integer;
  signal zxmwz : character;
begin
  gxmfiisjb : entity work.ctzgzeh
    port map (r => xz, nktl => zxmwz, huy => ysdz, tgaca => moezbukhpg);
  z : entity work.ctzgzeh
    port map (r => qcmjc, nktl => wcmnvlqzmm, huy => ysdz, tgaca => x);
end zrg;



entity lrfrsouehe is
  port (q : in character; tctafyajho : out time);
end lrfrsouehe;



architecture zwxpiozdk of lrfrsouehe is
  signal mi : time;
begin
  dhqzrwk : entity work.pjl
    port map (pwscemfrvc => mi, xz => tctafyajho);
  trluoddzy : entity work.pjl
    port map (pwscemfrvc => tctafyajho, xz => mi);
end zwxpiozdk;

library ieee;
use ieee.std_logic_1164.all;

entity znpwq is
  port (w : buffer time; wqpekzcnu : out std_logic; aabxd : linkage severity_level);
end znpwq;



architecture fwiik of znpwq is
  signal jmuqudtpd : time;
  signal ryez : character;
  signal gg : integer;
  signal xdszk : character;
  signal ajko : time;
begin
  szlrj : entity work.ctzgzeh
    port map (r => ajko, nktl => xdszk, huy => gg, tgaca => gg);
  zqua : entity work.lrfrsouehe
    port map (q => ryez, tctafyajho => jmuqudtpd);
  hovsszpw : entity work.lrfrsouehe
    port map (q => xdszk, tctafyajho => w);
  ydhmprfjxn : entity work.lrfrsouehe
    port map (q => xdszk, tctafyajho => ajko);
end fwiik;



-- Seed after: 3294752561216754839,3820899062418988741
