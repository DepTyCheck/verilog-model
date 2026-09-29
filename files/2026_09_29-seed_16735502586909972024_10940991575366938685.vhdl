-- Seed: 16735502586909972024,10940991575366938685

entity kenlgcswrf is
  port (lnkgvj : in real; nun : out time; aelztqe : out boolean_vector(4 to 0); rvg : in real);
end kenlgcswrf;

architecture euvzbi of kenlgcswrf is
  
begin
  -- Single-driven assignments
  aelztqe <= (others => TRUE);
  nun <= 16#4FC.5_1_7_7_0# ps;
end euvzbi;

library ieee;
use ieee.std_logic_1164.all;

entity xhnhoa is
  port (nnj : inout std_logic; wllfxpawf : linkage real; ppsdf : inout std_logic_vector(3 to 1));
end xhnhoa;

architecture qdciccnco of xhnhoa is
  signal fwnmztlh : real;
  signal ryfrqlm : boolean_vector(4 to 0);
  signal htupo : time;
  signal xllddcliub : real;
  signal awntijxamg : boolean_vector(4 to 0);
  signal u : time;
  signal jlnrzyguw : real;
begin
  i : entity work.kenlgcswrf
    port map (lnkgvj => jlnrzyguw, nun => u, aelztqe => awntijxamg, rvg => xllddcliub);
  d : entity work.kenlgcswrf
    port map (lnkgvj => xllddcliub, nun => htupo, aelztqe => ryfrqlm, rvg => fwnmztlh);
  
  -- Single-driven assignments
  xllddcliub <= jlnrzyguw;
  fwnmztlh <= 3_0_1.2333;
  jlnrzyguw <= 0.1_0_4_1_4;
  
  -- Multi-driven assignments
  nnj <= nnj;
end qdciccnco;

entity e is
  port (cxjummsxmd : out integer; iod : inout real; gfnvsijyv : inout severity_level; niaqg : linkage bit);
end e;

architecture tixcklsra of e is
  signal n : real;
  signal rzbjrzw : boolean_vector(4 to 0);
  signal wyscgm : time;
  signal jiozcoet : real;
  signal arzlwl : boolean_vector(4 to 0);
  signal lvjyagot : time;
  signal njhd : real;
  signal oiqje : boolean_vector(4 to 0);
  signal yuwwgb : time;
  signal jbxvnmvcvn : real;
begin
  vqaqwjyqc : entity work.kenlgcswrf
    port map (lnkgvj => jbxvnmvcvn, nun => yuwwgb, aelztqe => oiqje, rvg => njhd);
  knea : entity work.kenlgcswrf
    port map (lnkgvj => iod, nun => lvjyagot, aelztqe => arzlwl, rvg => iod);
  ssiavm : entity work.kenlgcswrf
    port map (lnkgvj => jiozcoet, nun => wyscgm, aelztqe => rzbjrzw, rvg => n);
  
  -- Single-driven assignments
  cxjummsxmd <= 13;
end tixcklsra;



-- Seed after: 15645478649999017621,10940991575366938685
