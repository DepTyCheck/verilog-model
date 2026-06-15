-- Seed: 6108562733006299545,15300320181035395489

entity aqfsbzoqo is
  port (nmckw : out time; gm : in real; bnxqev : linkage boolean; snkjg : buffer string(2 downto 3));
end aqfsbzoqo;

architecture bczufx of aqfsbzoqo is
  
begin
  
end bczufx;

library ieee;
use ieee.std_logic_1164.all;

entity zpcci is
  port (xvqyl : out std_logic; mybiwow : linkage boolean_vector(0 downto 0); nyobgq : inout boolean);
end zpcci;

architecture jdes of zpcci is
  signal wpyy : string(2 downto 3);
  signal ln : boolean;
  signal unts : real;
  signal fpehv : time;
begin
  x : entity work.aqfsbzoqo
    port map (nmckw => fpehv, gm => unts, bnxqev => ln, snkjg => wpyy);
  
  -- Single-driven assignments
  nyobgq <= TRUE;
  unts <= 32.41101;
end jdes;

entity dltfe is
  port (vvx : linkage real);
end dltfe;

architecture uzx of dltfe is
  signal eocxvemwg : string(2 downto 3);
  signal hnvhzb : boolean;
  signal vlzbgf : real;
  signal v : time;
  signal wz : string(2 downto 3);
  signal adhqdmysib : boolean;
  signal dt : real;
  signal yzlccxlq : time;
begin
  ikmhsjfp : entity work.aqfsbzoqo
    port map (nmckw => yzlccxlq, gm => dt, bnxqev => adhqdmysib, snkjg => wz);
  pekvjgaqg : entity work.aqfsbzoqo
    port map (nmckw => v, gm => vlzbgf, bnxqev => hnvhzb, snkjg => eocxvemwg);
  
  -- Single-driven assignments
  vlzbgf <= 2#110.1_1_0_0#;
  dt <= 2#01.11110#;
end uzx;

library ieee;
use ieee.std_logic_1164.all;

entity ssi is
  port (r : inout boolean_vector(1 to 3); d : inout real; bqwphc : inout std_logic_vector(2 to 3));
end ssi;

architecture o of ssi is
  signal bevfzvge : string(2 downto 3);
  signal cxcrsujs : boolean;
  signal qugkkboem : real;
  signal parnarywi : time;
  signal atsawrljrq : string(2 downto 3);
  signal atoejb : boolean;
  signal mcb : real;
  signal m : time;
begin
  jhksiksgfa : entity work.aqfsbzoqo
    port map (nmckw => m, gm => mcb, bnxqev => atoejb, snkjg => atsawrljrq);
  bmyngda : entity work.aqfsbzoqo
    port map (nmckw => parnarywi, gm => qugkkboem, bnxqev => cxcrsujs, snkjg => bevfzvge);
  
  -- Single-driven assignments
  r <= (FALSE, FALSE, TRUE);
  d <= 304.213;
  qugkkboem <= 2#00.1001#;
  
  -- Multi-driven assignments
  bqwphc <= "W1";
  bqwphc <= ('Z', 'Z');
end o;



-- Seed after: 6138518598173460704,15300320181035395489
