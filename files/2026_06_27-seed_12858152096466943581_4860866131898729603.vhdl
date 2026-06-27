-- Seed: 12858152096466943581,4860866131898729603

entity jfjeocdi is
  port (jqpgioeoz : in time; yyqhbuuj : out real);
end jfjeocdi;

architecture rainqe of jfjeocdi is
  
begin
  -- Single-driven assignments
  yyqhbuuj <= 3_0.3_1;
end rainqe;

library ieee;
use ieee.std_logic_1164.all;

entity ypkxedhcp is
  port (jmznmxjny : linkage std_logic_vector(2 downto 1); kcwvamb : inout character; yupev : out std_logic_vector(4 to 0); bqczfikfq : out real);
end ypkxedhcp;

architecture yphn of ypkxedhcp is
  signal i : real;
  signal zjcz : real;
  signal hffrhnrn : time;
begin
  vovbxeljp : entity work.jfjeocdi
    port map (jqpgioeoz => hffrhnrn, yyqhbuuj => zjcz);
  imeuiafrbi : entity work.jfjeocdi
    port map (jqpgioeoz => hffrhnrn, yyqhbuuj => i);
  kjmtwyd : entity work.jfjeocdi
    port map (jqpgioeoz => hffrhnrn, yyqhbuuj => bqczfikfq);
  
  -- Single-driven assignments
  kcwvamb <= 't';
  hffrhnrn <= 0 hr;
  
  -- Multi-driven assignments
  yupev <= "";
  yupev <= (others => '0');
  yupev <= "";
end yphn;

library ieee;
use ieee.std_logic_1164.all;

entity lpn is
  port (futeo : in std_logic; ozpu : buffer std_logic; fsrgol : linkage std_logic_vector(4 to 4); wcjlv : inout std_logic_vector(0 downto 1));
end lpn;

architecture x of lpn is
  signal kfczaxbgn : real;
  signal vmy : time;
  signal dnlqqzaq : real;
  signal b : real;
  signal qldtaqbi : time;
begin
  aafcz : entity work.jfjeocdi
    port map (jqpgioeoz => qldtaqbi, yyqhbuuj => b);
  pagyhqbo : entity work.jfjeocdi
    port map (jqpgioeoz => qldtaqbi, yyqhbuuj => dnlqqzaq);
  ovkzrcns : entity work.jfjeocdi
    port map (jqpgioeoz => vmy, yyqhbuuj => kfczaxbgn);
  
  -- Multi-driven assignments
  ozpu <= 'X';
end x;

entity tqybcoenw is
  port (bgz : buffer bit_vector(0 to 3); qauqynfrvg : linkage real);
end tqybcoenw;

library ieee;
use ieee.std_logic_1164.all;

architecture oscjkmf of tqybcoenw is
  signal ym : real;
  signal s : time;
  signal sjsydvka : std_logic_vector(4 to 4);
  signal mruolqkpir : std_logic;
  signal vbvcvj : std_logic;
  signal vlf : real;
  signal kmpan : std_logic_vector(0 downto 1);
  signal ueqnog : character;
  signal imkyzwlgav : std_logic_vector(2 downto 1);
begin
  syhxldlapk : entity work.ypkxedhcp
    port map (jmznmxjny => imkyzwlgav, kcwvamb => ueqnog, yupev => kmpan, bqczfikfq => vlf);
  uvqebj : entity work.lpn
    port map (futeo => vbvcvj, ozpu => mruolqkpir, fsrgol => sjsydvka, wcjlv => kmpan);
  sycqb : entity work.jfjeocdi
    port map (jqpgioeoz => s, yyqhbuuj => ym);
  
  -- Single-driven assignments
  bgz <= ('1', '1', '0', '0');
  s <= 3_2_3_0.2120 ps;
  
  -- Multi-driven assignments
  vbvcvj <= 'X';
end oscjkmf;



-- Seed after: 10361955043552998237,4860866131898729603
