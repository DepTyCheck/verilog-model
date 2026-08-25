-- Seed: 16889177737850791262,13501862637168280927

entity woslr is
  port (repxlgt : linkage severity_level; egkrw : out integer; ec : buffer time; iovtnqllji : out real_vector(1 downto 4));
end woslr;

architecture wxelhe of woslr is
  
begin
  
end wxelhe;

library ieee;
use ieee.std_logic_1164.all;

entity utlpdr is
  port (cwa : inout real; birehbwd : in time; oygekcjzqf : linkage string(5 to 2); jfftywsqrc : linkage std_logic_vector(3 to 2));
end utlpdr;

architecture zxblqbb of utlpdr is
  signal ltvuua : real_vector(1 downto 4);
  signal yai : time;
  signal rvzmfqsk : integer;
  signal h : severity_level;
begin
  jrdtsbym : entity work.woslr
    port map (repxlgt => h, egkrw => rvzmfqsk, ec => yai, iovtnqllji => ltvuua);
  
  -- Single-driven assignments
  cwa <= 4_4_4.2;
end zxblqbb;

library ieee;
use ieee.std_logic_1164.all;

entity lp is
  port (ppuas : linkage std_logic_vector(2 downto 4); eadpjc : in std_logic);
end lp;

library ieee;
use ieee.std_logic_1164.all;

architecture pnlqev of lp is
  signal ixm : real_vector(1 downto 4);
  signal czvp : time;
  signal lqelnujl : integer;
  signal fy : severity_level;
  signal gllai : std_logic_vector(3 to 2);
  signal v : string(5 to 2);
  signal dprc : time;
  signal npriqan : real;
  signal dfofqpmrzy : real_vector(1 downto 4);
  signal efdp : time;
  signal bxjoxcf : integer;
  signal hcvtbawrg : severity_level;
  signal luashz : real_vector(1 downto 4);
  signal cyzteo : time;
  signal ciygu : integer;
  signal ccmtfu : severity_level;
begin
  skgipy : entity work.woslr
    port map (repxlgt => ccmtfu, egkrw => ciygu, ec => cyzteo, iovtnqllji => luashz);
  m : entity work.woslr
    port map (repxlgt => hcvtbawrg, egkrw => bxjoxcf, ec => efdp, iovtnqllji => dfofqpmrzy);
  yftnows : entity work.utlpdr
    port map (cwa => npriqan, birehbwd => dprc, oygekcjzqf => v, jfftywsqrc => gllai);
  e : entity work.woslr
    port map (repxlgt => fy, egkrw => lqelnujl, ec => czvp, iovtnqllji => ixm);
  
  -- Multi-driven assignments
  gllai <= "";
  gllai <= gllai;
  gllai <= (others => '0');
  gllai <= gllai;
end pnlqev;



-- Seed after: 13354576200737561175,13501862637168280927
