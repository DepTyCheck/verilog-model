-- Seed: 16825178805471021001,9125939553767483053

library ieee;
use ieee.std_logic_1164.all;

entity ixu is
  port (zzc : linkage std_logic; ephvgoj : buffer boolean; rtkzcuxmot : out real_vector(2 downto 4); ekfvjxqhmq : linkage time);
end ixu;



architecture wcqlpc of ixu is
  
begin
  
end wcqlpc;

library ieee;
use ieee.std_logic_1164.all;

entity grcan is
  port (evxcmuq : linkage std_logic_vector(0 downto 3); kvixueuc : out std_logic; hv : in real; s : buffer bit);
end grcan;



architecture dco of grcan is
  signal vfh : time;
  signal k : real_vector(2 downto 4);
  signal yemseap : boolean;
  signal dosy : time;
  signal n : real_vector(2 downto 4);
  signal mih : boolean;
begin
  bggrncl : entity work.ixu
    port map (zzc => kvixueuc, ephvgoj => mih, rtkzcuxmot => n, ekfvjxqhmq => dosy);
  djzctdk : entity work.ixu
    port map (zzc => kvixueuc, ephvgoj => yemseap, rtkzcuxmot => k, ekfvjxqhmq => vfh);
end dco;

library ieee;
use ieee.std_logic_1164.all;

entity mwhzcchn is
  port (hbol : linkage integer_vector(1 downto 4); rifha : buffer std_logic; x : linkage integer_vector(4 to 0));
end mwhzcchn;

library ieee;
use ieee.std_logic_1164.all;

architecture nngsojx of mwhzcchn is
  signal meuxeee : real_vector(2 downto 4);
  signal cv : boolean;
  signal umbm : std_logic;
  signal reinnil : time;
  signal v : real_vector(2 downto 4);
  signal al : boolean;
  signal bnecpixhd : std_logic;
  signal pgeofajb : bit;
  signal zudksndij : real;
  signal anihywewzj : std_logic;
  signal vdm : std_logic_vector(0 downto 3);
begin
  muwc : entity work.grcan
    port map (evxcmuq => vdm, kvixueuc => anihywewzj, hv => zudksndij, s => pgeofajb);
  tzroextcby : entity work.ixu
    port map (zzc => bnecpixhd, ephvgoj => al, rtkzcuxmot => v, ekfvjxqhmq => reinnil);
  iqy : entity work.ixu
    port map (zzc => umbm, ephvgoj => cv, rtkzcuxmot => meuxeee, ekfvjxqhmq => reinnil);
end nngsojx;

library ieee;
use ieee.std_logic_1164.all;

entity bzgdwkde is
  port (dtsdlef : linkage std_logic; prgfc : inout std_logic_vector(3 to 1));
end bzgdwkde;

library ieee;
use ieee.std_logic_1164.all;

architecture bn of bzgdwkde is
  signal aultutc : bit;
  signal sl : real;
  signal tisglajv : std_logic;
  signal ltlsc : std_logic_vector(0 downto 3);
  signal sfxryw : time;
  signal spaplx : real_vector(2 downto 4);
  signal gueozd : boolean;
  signal yzggjl : std_logic;
begin
  bt : entity work.ixu
    port map (zzc => yzggjl, ephvgoj => gueozd, rtkzcuxmot => spaplx, ekfvjxqhmq => sfxryw);
  exrixfg : entity work.grcan
    port map (evxcmuq => ltlsc, kvixueuc => tisglajv, hv => sl, s => aultutc);
end bn;



-- Seed after: 3607170852041788339,9125939553767483053
