-- Seed: 7986032717753209516,16461708287571398341

entity klspxige is
  port (gkgchh : in real; ioq : in time);
end klspxige;

architecture kc of klspxige is
  
begin
  
end kc;

library ieee;
use ieee.std_logic_1164.all;

entity zh is
  port (topzhi : in real; cqilp : in std_logic; mrgc : in std_logic_vector(0 to 3); cblqzr : linkage boolean);
end zh;

architecture upzbvl of zh is
  signal eknkcx : time;
  signal ibvniwkobx : real;
  signal gywxystxy : time;
  signal c : real;
  signal ebpduujyqz : time;
  signal wvdvhp : real;
begin
  kikqdgnh : entity work.klspxige
    port map (gkgchh => wvdvhp, ioq => ebpduujyqz);
  itpj : entity work.klspxige
    port map (gkgchh => c, ioq => ebpduujyqz);
  jdfbzuygdg : entity work.klspxige
    port map (gkgchh => c, ioq => gywxystxy);
  quwparh : entity work.klspxige
    port map (gkgchh => ibvniwkobx, ioq => eknkcx);
  
  -- Single-driven assignments
  wvdvhp <= 16#67.C#;
end upzbvl;

library ieee;
use ieee.std_logic_1164.all;

entity uilgnpd is
  port (gx : linkage time; dztxzbb : out real; zgamllzml : out std_logic_vector(0 to 3); d : out bit);
end uilgnpd;

library ieee;
use ieee.std_logic_1164.all;

architecture iyr of uilgnpd is
  signal xdlbak : boolean;
  signal nrsis : std_logic_vector(0 to 3);
  signal glmddkgpnh : std_logic;
  signal apdnkffh : real;
  signal yegzckkbvf : time;
begin
  ngimr : entity work.klspxige
    port map (gkgchh => dztxzbb, ioq => yegzckkbvf);
  mbsms : entity work.zh
    port map (topzhi => apdnkffh, cqilp => glmddkgpnh, mrgc => nrsis, cblqzr => xdlbak);
  
  -- Single-driven assignments
  dztxzbb <= 2.00300;
  yegzckkbvf <= 4 sec;
  d <= '1';
  
  -- Multi-driven assignments
  nrsis <= zgamllzml;
  zgamllzml <= "LLZ0";
end iyr;

library ieee;
use ieee.std_logic_1164.all;

entity cgfvs is
  port (jbjsencllz : in time; qewstko : linkage std_logic_vector(3 to 4); snwqimz : in character);
end cgfvs;

library ieee;
use ieee.std_logic_1164.all;

architecture cicmemiwux of cgfvs is
  signal mirvgzecdo : time;
  signal l : real;
  signal uqfsv : time;
  signal oarcdd : real;
  signal tme : real;
  signal btetc : bit;
  signal mmc : std_logic_vector(0 to 3);
  signal yjznmykqwx : real;
  signal vfvqglogau : time;
begin
  isyvqrj : entity work.uilgnpd
    port map (gx => vfvqglogau, dztxzbb => yjznmykqwx, zgamllzml => mmc, d => btetc);
  e : entity work.klspxige
    port map (gkgchh => tme, ioq => jbjsencllz);
  gjufe : entity work.klspxige
    port map (gkgchh => oarcdd, ioq => uqfsv);
  j : entity work.klspxige
    port map (gkgchh => l, ioq => mirvgzecdo);
  
  -- Single-driven assignments
  tme <= yjznmykqwx;
  mirvgzecdo <= 8#101# ns;
  l <= yjznmykqwx;
  
  -- Multi-driven assignments
  mmc <= "LZL-";
  mmc <= mmc;
  mmc <= ('0', '1', 'X', 'W');
  mmc <= ('W', 'X', 'H', '-');
end cicmemiwux;



-- Seed after: 18065729587608155054,16461708287571398341
