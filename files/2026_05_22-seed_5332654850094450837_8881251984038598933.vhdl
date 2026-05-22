-- Seed: 5332654850094450837,8881251984038598933



entity r is
  port (w : linkage real; vyyikdxzqo : inout boolean);
end r;



architecture dyrjj of r is
  
begin
  
end dyrjj;

library ieee;
use ieee.std_logic_1164.all;

entity dhkgeyhdc is
  port (uuzr : out std_logic; wgtimdar : linkage bit);
end dhkgeyhdc;



architecture omaurmeuuu of dhkgeyhdc is
  signal dryyh : boolean;
  signal jqlaesrmix : boolean;
  signal gsioq : real;
begin
  nrihgee : entity work.r
    port map (w => gsioq, vyyikdxzqo => jqlaesrmix);
  tkgj : entity work.r
    port map (w => gsioq, vyyikdxzqo => dryyh);
end omaurmeuuu;

library ieee;
use ieee.std_logic_1164.all;

entity rgjly is
  port (ezvpsoa : out std_logic; up : buffer real);
end rgjly;

library ieee;
use ieee.std_logic_1164.all;

architecture mithrqmc of rgjly is
  signal lgk : boolean;
  signal ajzmbkn : bit;
  signal rvh : bit;
  signal z : std_logic;
  signal nksdhldwv : boolean;
begin
  sxxslapxj : entity work.r
    port map (w => up, vyyikdxzqo => nksdhldwv);
  c : entity work.dhkgeyhdc
    port map (uuzr => z, wgtimdar => rvh);
  zljw : entity work.dhkgeyhdc
    port map (uuzr => z, wgtimdar => ajzmbkn);
  qz : entity work.r
    port map (w => up, vyyikdxzqo => lgk);
end mithrqmc;



entity zq is
  port (qqwlba : buffer real; sodi : out real; sds : in character);
end zq;

library ieee;
use ieee.std_logic_1164.all;

architecture blsef of zq is
  signal kcdgk : std_logic;
  signal xfzlytmg : boolean;
  signal vdbt : real;
  signal jsm : bit;
  signal lxeaqbi : std_logic;
begin
  lqr : entity work.rgjly
    port map (ezvpsoa => lxeaqbi, up => qqwlba);
  v : entity work.dhkgeyhdc
    port map (uuzr => lxeaqbi, wgtimdar => jsm);
  j : entity work.r
    port map (w => vdbt, vyyikdxzqo => xfzlytmg);
  evufb : entity work.dhkgeyhdc
    port map (uuzr => kcdgk, wgtimdar => jsm);
end blsef;



-- Seed after: 13807155948062869028,8881251984038598933
