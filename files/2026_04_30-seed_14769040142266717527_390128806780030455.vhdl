-- Seed: 14769040142266717527,390128806780030455



entity kpyickzqrq is
  port (xiwnnqb : inout real; gx : inout real);
end kpyickzqrq;



architecture yhwjejbl of kpyickzqrq is
  
begin
  
end yhwjejbl;

library ieee;
use ieee.std_logic_1164.all;

entity ywictcy is
  port (hujv : linkage std_logic; fhetuja : in boolean);
end ywictcy;



architecture fldlejtfr of ywictcy is
  signal y : real;
  signal znxv : real;
  signal rt : real;
  signal ox : real;
begin
  rinzgy : entity work.kpyickzqrq
    port map (xiwnnqb => ox, gx => rt);
  nexj : entity work.kpyickzqrq
    port map (xiwnnqb => znxv, gx => y);
end fldlejtfr;



entity fvauljibn is
  port (xgxz : out boolean; ufwrk : out time);
end fvauljibn;

library ieee;
use ieee.std_logic_1164.all;

architecture i of fvauljibn is
  signal jaemxbaqe : boolean;
  signal senb : std_logic;
begin
  kf : entity work.ywictcy
    port map (hujv => senb, fhetuja => jaemxbaqe);
end i;



entity qlgewecdff is
  port (bclopwnyru : buffer time);
end qlgewecdff;

library ieee;
use ieee.std_logic_1164.all;

architecture qzfypiildz of qlgewecdff is
  signal ampui : real;
  signal iseozzhws : real;
  signal odzex : time;
  signal jk : boolean;
  signal ezmnadbqm : std_logic;
begin
  sooedws : entity work.ywictcy
    port map (hujv => ezmnadbqm, fhetuja => jk);
  yymcnhlxp : entity work.fvauljibn
    port map (xgxz => jk, ufwrk => odzex);
  dkbj : entity work.kpyickzqrq
    port map (xiwnnqb => iseozzhws, gx => ampui);
end qzfypiildz;



-- Seed after: 2794430752996597868,390128806780030455
