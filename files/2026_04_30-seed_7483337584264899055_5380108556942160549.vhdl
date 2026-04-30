-- Seed: 7483337584264899055,5380108556942160549



entity qceysvsg is
  port (hdfdv : out real; iqmdpxyrxn : in integer; gnqis : out time; dt : inout real);
end qceysvsg;



architecture yjmt of qceysvsg is
  
begin
  
end yjmt;



entity pzox is
  port (mtdzhqgsjp : in integer);
end pzox;



architecture p of pzox is
  signal btirkwzoo : real;
  signal nltd : time;
  signal eczyhgp : real;
  signal sbfdjuhlj : real;
  signal jr : time;
  signal xewqipf : real;
begin
  kcjyzsyv : entity work.qceysvsg
    port map (hdfdv => xewqipf, iqmdpxyrxn => mtdzhqgsjp, gnqis => jr, dt => sbfdjuhlj);
  slilfpeey : entity work.qceysvsg
    port map (hdfdv => eczyhgp, iqmdpxyrxn => mtdzhqgsjp, gnqis => nltd, dt => btirkwzoo);
end p;

library ieee;
use ieee.std_logic_1164.all;

entity aaz is
  port (m : buffer std_logic; bkpxdtyo : out integer);
end aaz;



architecture icw of aaz is
  signal fffsu : real;
  signal obgcxofes : time;
  signal orse : real;
  signal yywnnnlgtw : real;
  signal y : time;
  signal xoonk : integer;
  signal ficuqlqi : real;
begin
  ynilcmrtwd : entity work.qceysvsg
    port map (hdfdv => ficuqlqi, iqmdpxyrxn => xoonk, gnqis => y, dt => yywnnnlgtw);
  rfidd : entity work.qceysvsg
    port map (hdfdv => orse, iqmdpxyrxn => bkpxdtyo, gnqis => obgcxofes, dt => fffsu);
end icw;



entity pws is
  port (lgqfc : inout integer);
end pws;



architecture edp of pws is
  signal oorlijwlvk : integer;
  signal ro : real;
  signal pg : time;
  signal ome : integer;
  signal pstqfasnny : real;
begin
  kwcsfnpmwx : entity work.qceysvsg
    port map (hdfdv => pstqfasnny, iqmdpxyrxn => ome, gnqis => pg, dt => ro);
  x : entity work.pzox
    port map (mtdzhqgsjp => oorlijwlvk);
end edp;



-- Seed after: 17568586510097356095,5380108556942160549
