-- Seed: 11371809607358373912,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity aqgznubn is
  port (cwqifzugat : inout std_logic_vector(2 to 1); a : in bit);
end aqgznubn;

architecture fw of aqgznubn is
  
begin
  -- Multi-driven assignments
  cwqifzugat <= (others => '0');
end fw;

library ieee;
use ieee.std_logic_1164.all;

entity lqxurrujp is
  port (zzcmzbvk : out std_logic_vector(0 to 3); i : linkage time; o : linkage time; suxcsau : linkage real);
end lqxurrujp;

library ieee;
use ieee.std_logic_1164.all;

architecture jmm of lqxurrujp is
  signal mpzdmjkzl : std_logic_vector(2 to 1);
  signal aocwf : bit;
  signal mhrwhskd : std_logic_vector(2 to 1);
begin
  ftlqyq : entity work.aqgznubn
    port map (cwqifzugat => mhrwhskd, a => aocwf);
  ica : entity work.aqgznubn
    port map (cwqifzugat => mhrwhskd, a => aocwf);
  vsbt : entity work.aqgznubn
    port map (cwqifzugat => mpzdmjkzl, a => aocwf);
  
  -- Single-driven assignments
  aocwf <= '0';
  
  -- Multi-driven assignments
  zzcmzbvk <= "W-0U";
end jmm;

entity bjyakdnc is
  port (uz : linkage time);
end bjyakdnc;

library ieee;
use ieee.std_logic_1164.all;

architecture wymcyej of bjyakdnc is
  signal zuojatqlj : std_logic_vector(2 to 1);
  signal qyzrr : real;
  signal bzgzyz : time;
  signal prmjil : time;
  signal o : std_logic_vector(0 to 3);
  signal psei : bit;
  signal gpojjvk : std_logic_vector(2 to 1);
begin
  ge : entity work.aqgznubn
    port map (cwqifzugat => gpojjvk, a => psei);
  xi : entity work.lqxurrujp
    port map (zzcmzbvk => o, i => prmjil, o => bzgzyz, suxcsau => qyzrr);
  lwtozsdfnn : entity work.aqgznubn
    port map (cwqifzugat => zuojatqlj, a => psei);
  
  -- Single-driven assignments
  psei <= '1';
  
  -- Multi-driven assignments
  gpojjvk <= gpojjvk;
  gpojjvk <= gpojjvk;
  o <= "L-1U";
end wymcyej;

entity ehycndzmip is
  port (nbzfiokz : out bit);
end ehycndzmip;

library ieee;
use ieee.std_logic_1164.all;

architecture wree of ehycndzmip is
  signal ycs : std_logic_vector(2 to 1);
  signal ukyqtcp : time;
begin
  mun : entity work.bjyakdnc
    port map (uz => ukyqtcp);
  gim : entity work.aqgznubn
    port map (cwqifzugat => ycs, a => nbzfiokz);
  
  -- Single-driven assignments
  nbzfiokz <= nbzfiokz;
  
  -- Multi-driven assignments
  ycs <= (others => '0');
  ycs <= "";
  ycs <= "";
end wree;



-- Seed after: 393729204842800367,14641901754878719179
