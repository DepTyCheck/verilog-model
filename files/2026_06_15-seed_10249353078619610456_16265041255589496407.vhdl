-- Seed: 10249353078619610456,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (u : linkage std_logic; acdche : linkage integer; nfvtnyyaj : out time_vector(2 downto 4); pqf : linkage time);
end j;



architecture iaq of j is
  
begin
  
end iaq;

library ieee;
use ieee.std_logic_1164.all;

entity iwuxdp is
  port (zvchbjp : linkage std_logic; sfqflkouf : linkage bit_vector(4 to 3); gvropp : linkage integer_vector(1 to 2); gykmzgbke : inout integer);
end iwuxdp;

library ieee;
use ieee.std_logic_1164.all;

architecture od of iwuxdp is
  signal cuv : time;
  signal ksvdtd : time_vector(2 downto 4);
  signal n : std_logic;
begin
  hh : entity work.j
    port map (u => n, acdche => gykmzgbke, nfvtnyyaj => ksvdtd, pqf => cuv);
end od;



entity zntk is
  port (tojhz : buffer severity_level; vbefycgnvu : out time; neojav : out time);
end zntk;

library ieee;
use ieee.std_logic_1164.all;

architecture yg of zntk is
  signal rgbfkneki : time;
  signal rzfjipwdog : time_vector(2 downto 4);
  signal domfot : integer;
  signal xngctyyff : std_logic;
  signal btrbpbjpi : time;
  signal telkvjl : time_vector(2 downto 4);
  signal imtspnra : std_logic;
  signal upmhgousmc : integer_vector(1 to 2);
  signal wg : bit_vector(4 to 3);
  signal dy : std_logic;
  signal dtuadm : time_vector(2 downto 4);
  signal z : integer;
  signal blfiq : std_logic;
begin
  argkwy : entity work.j
    port map (u => blfiq, acdche => z, nfvtnyyaj => dtuadm, pqf => neojav);
  cmggxmm : entity work.iwuxdp
    port map (zvchbjp => dy, sfqflkouf => wg, gvropp => upmhgousmc, gykmzgbke => z);
  zfh : entity work.j
    port map (u => imtspnra, acdche => z, nfvtnyyaj => telkvjl, pqf => btrbpbjpi);
  ljbq : entity work.j
    port map (u => xngctyyff, acdche => domfot, nfvtnyyaj => rzfjipwdog, pqf => rgbfkneki);
end yg;

library ieee;
use ieee.std_logic_1164.all;

entity audmmapock is
  port (kz : linkage std_logic_vector(3 downto 4));
end audmmapock;

library ieee;
use ieee.std_logic_1164.all;

architecture qj of audmmapock is
  signal fvgbc : time;
  signal g : time_vector(2 downto 4);
  signal esgsxuhssy : time_vector(2 downto 4);
  signal lgjqyfbphk : std_logic;
  signal bqyngexkw : time;
  signal lhg : time_vector(2 downto 4);
  signal xypjlmdcyy : integer;
  signal ojbj : std_logic;
  signal jkmmmtm : time;
  signal qfr : time;
  signal fnoyiayoht : severity_level;
begin
  upju : entity work.zntk
    port map (tojhz => fnoyiayoht, vbefycgnvu => qfr, neojav => jkmmmtm);
  hielzmei : entity work.j
    port map (u => ojbj, acdche => xypjlmdcyy, nfvtnyyaj => lhg, pqf => bqyngexkw);
  vflxpr : entity work.j
    port map (u => lgjqyfbphk, acdche => xypjlmdcyy, nfvtnyyaj => esgsxuhssy, pqf => qfr);
  d : entity work.j
    port map (u => ojbj, acdche => xypjlmdcyy, nfvtnyyaj => g, pqf => fvgbc);
end qj;



-- Seed after: 13828104372808462330,16265041255589496407
