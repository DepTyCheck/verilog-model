-- Seed: 7462572574479489526,10875537289884587119

entity vmyu is
  port (ypbk : inout time; vykbiaghav : buffer time; gvw : in boolean; ujjuhxybsj : linkage severity_level);
end vmyu;

architecture yshwuphi of vmyu is
  
begin
  -- Single-driven assignments
  vykbiaghav <= vykbiaghav;
  ypbk <= vykbiaghav;
end yshwuphi;

library ieee;
use ieee.std_logic_1164.all;

entity cp is
  port (gzakjvfhj : out integer; mumf : inout std_logic);
end cp;

architecture wcdbos of cp is
  signal pzutb : severity_level;
  signal usdx : time;
  signal pgqj : time;
  signal qwdh : severity_level;
  signal qbfg : boolean;
  signal kkbzju : time;
  signal nuwhactkld : time;
  signal lhuhhsadgb : severity_level;
  signal oplq : boolean;
  signal ouifiirz : time;
  signal wccvanrdk : time;
begin
  sxpzv : entity work.vmyu
    port map (ypbk => wccvanrdk, vykbiaghav => ouifiirz, gvw => oplq, ujjuhxybsj => lhuhhsadgb);
  de : entity work.vmyu
    port map (ypbk => nuwhactkld, vykbiaghav => kkbzju, gvw => qbfg, ujjuhxybsj => qwdh);
  l : entity work.vmyu
    port map (ypbk => pgqj, vykbiaghav => usdx, gvw => qbfg, ujjuhxybsj => pzutb);
  
  -- Single-driven assignments
  qbfg <= FALSE;
  oplq <= TRUE;
  gzakjvfhj <= 16#64D40#;
  
  -- Multi-driven assignments
  mumf <= 'U';
end wcdbos;



-- Seed after: 9464559815568270532,10875537289884587119
