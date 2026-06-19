-- Seed: 12211035096855295816,3108530264173481209

entity qhxoecgnjl is
  port (xalvnadvd : in real);
end qhxoecgnjl;

architecture n of qhxoecgnjl is
  
begin
  
end n;

entity nxtubh is
  port (zghwnwmmc : in time_vector(2 downto 0); lpcpxttdkx : inout boolean);
end nxtubh;

architecture pkrwi of nxtubh is
  
begin
  -- Single-driven assignments
  lpcpxttdkx <= TRUE;
end pkrwi;

entity dzhbvo is
  port (kjfiijv : buffer integer; ymouqog : in time; woe : buffer boolean_vector(4 downto 2));
end dzhbvo;

architecture qycnletg of dzhbvo is
  signal u : boolean;
  signal yqcm : time_vector(2 downto 0);
  signal qifbt : real;
begin
  ucjqcyrzji : entity work.qhxoecgnjl
    port map (xalvnadvd => qifbt);
  hag : entity work.nxtubh
    port map (zghwnwmmc => yqcm, lpcpxttdkx => u);
end qycnletg;

library ieee;
use ieee.std_logic_1164.all;

entity gdyxvq is
  port (gyppjp : in time; jtsskqo : in std_logic_vector(4 downto 4));
end gdyxvq;

architecture kdiv of gdyxvq is
  signal ermvqj : real;
  signal b : real;
  signal lgwmwjk : boolean_vector(4 downto 2);
  signal lsw : integer;
begin
  mntkocq : entity work.dzhbvo
    port map (kjfiijv => lsw, ymouqog => gyppjp, woe => lgwmwjk);
  cjhqu : entity work.qhxoecgnjl
    port map (xalvnadvd => b);
  vgixcli : entity work.qhxoecgnjl
    port map (xalvnadvd => ermvqj);
  
  -- Single-driven assignments
  b <= 2_3_2_0.1_1;
  ermvqj <= 3_1_1_3_3.321;
end kdiv;



-- Seed after: 1179738237767498844,3108530264173481209
