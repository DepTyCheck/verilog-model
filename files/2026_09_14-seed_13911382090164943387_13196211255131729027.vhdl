-- Seed: 13911382090164943387,13196211255131729027

entity iqffyje is
  port (qhpbdzzwza : in integer; ztauasifn : out boolean_vector(2 to 4); g : buffer real; hddr : in boolean);
end iqffyje;

architecture grwthgyf of iqffyje is
  
begin
  
end grwthgyf;

library ieee;
use ieee.std_logic_1164.all;

entity yuqau is
  port (w : out std_logic_vector(4 downto 1));
end yuqau;

architecture ijvdlg of yuqau is
  signal vbqwkyym : boolean;
  signal dwucezmy : real;
  signal ikui : boolean_vector(2 to 4);
  signal viv : real;
  signal rzygpnj : boolean_vector(2 to 4);
  signal orlhzxsriy : integer;
  signal rrlgq : boolean;
  signal ouaqyrdml : real;
  signal xahkdrvafp : boolean_vector(2 to 4);
  signal wpetsga : integer;
begin
  vqcjyh : entity work.iqffyje
    port map (qhpbdzzwza => wpetsga, ztauasifn => xahkdrvafp, g => ouaqyrdml, hddr => rrlgq);
  vrfudu : entity work.iqffyje
    port map (qhpbdzzwza => orlhzxsriy, ztauasifn => rzygpnj, g => viv, hddr => rrlgq);
  jnavchd : entity work.iqffyje
    port map (qhpbdzzwza => wpetsga, ztauasifn => ikui, g => dwucezmy, hddr => vbqwkyym);
  
  -- Single-driven assignments
  wpetsga <= 2#001#;
  
  -- Multi-driven assignments
  w <= ('0', '0', 'L', '-');
  w <= ('W', '-', 'H', 'H');
  w <= "UXH0";
end ijvdlg;

entity rzg is
  port (gxre : out real; wzshqcfluk : inout integer);
end rzg;

library ieee;
use ieee.std_logic_1164.all;

architecture vsd of rzg is
  signal xsfa : boolean;
  signal qlpognxu : real;
  signal cbgdglo : boolean_vector(2 to 4);
  signal ihxtub : std_logic_vector(4 downto 1);
  signal vel : boolean;
  signal pybasp : boolean_vector(2 to 4);
begin
  jkkevdqtgc : entity work.iqffyje
    port map (qhpbdzzwza => wzshqcfluk, ztauasifn => pybasp, g => gxre, hddr => vel);
  ec : entity work.yuqau
    port map (w => ihxtub);
  wpitchs : entity work.iqffyje
    port map (qhpbdzzwza => wzshqcfluk, ztauasifn => cbgdglo, g => qlpognxu, hddr => xsfa);
  
  -- Single-driven assignments
  wzshqcfluk <= wzshqcfluk;
  xsfa <= TRUE;
  vel <= vel;
end vsd;



-- Seed after: 849415115367825186,13196211255131729027
