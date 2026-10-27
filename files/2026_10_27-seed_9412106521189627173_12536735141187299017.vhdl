-- Seed: 9412106521189627173,12536735141187299017

entity bnnzylrj is
  port (flevztvgn : buffer integer_vector(4 to 2); yozutv : in string(3 to 3); vwqhzaj : inout time);
end bnnzylrj;

architecture xrvgd of bnnzylrj is
  
begin
  -- Single-driven assignments
  vwqhzaj <= 16#D_5_7# ns;
  flevztvgn <= flevztvgn;
end xrvgd;

entity i is
  port (xdhlolcgl : linkage time; nxtmpatx : linkage integer_vector(1 downto 3); ogyicdkhn : out integer_vector(3 to 2); wxc : inout time);
end i;

architecture lxpcyqeqr of i is
  signal dt : time;
  signal lyhaczdy : string(3 to 3);
  signal ncmkrogc : integer_vector(4 to 2);
  signal eoghn : string(3 to 3);
  signal qzrjorgy : integer_vector(4 to 2);
begin
  ubjaa : entity work.bnnzylrj
    port map (flevztvgn => qzrjorgy, yozutv => eoghn, vwqhzaj => wxc);
  hemdg : entity work.bnnzylrj
    port map (flevztvgn => ncmkrogc, yozutv => lyhaczdy, vwqhzaj => dt);
  
  -- Single-driven assignments
  lyhaczdy <= eoghn;
  eoghn <= eoghn;
  ogyicdkhn <= (others => 0);
end lxpcyqeqr;

library ieee;
use ieee.std_logic_1164.all;

entity jmycj is
  port (svwtoy : buffer boolean; brbalv : linkage real_vector(0 downto 4); vlt : inout std_logic; cqbazkj : inout bit_vector(4 to 3));
end jmycj;

architecture em of jmycj is
  signal rhhu : time;
  signal teavkosnkh : integer_vector(4 to 2);
  signal wc : time;
  signal yagty : string(3 to 3);
  signal bhxieqgs : integer_vector(4 to 2);
  signal w : time;
  signal bbvqlw : string(3 to 3);
  signal qxrtptsfb : integer_vector(4 to 2);
begin
  o : entity work.bnnzylrj
    port map (flevztvgn => qxrtptsfb, yozutv => bbvqlw, vwqhzaj => w);
  xiynzz : entity work.bnnzylrj
    port map (flevztvgn => bhxieqgs, yozutv => yagty, vwqhzaj => wc);
  qp : entity work.bnnzylrj
    port map (flevztvgn => teavkosnkh, yozutv => bbvqlw, vwqhzaj => rhhu);
  
  -- Multi-driven assignments
  vlt <= 'W';
  vlt <= vlt;
  vlt <= vlt;
end em;



-- Seed after: 3987231690776935912,12536735141187299017
