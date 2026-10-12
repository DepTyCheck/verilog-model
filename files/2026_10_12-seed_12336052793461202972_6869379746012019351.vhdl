-- Seed: 12336052793461202972,6869379746012019351

entity cdkqjhqq is
  port (myoomxnk : linkage string(2 downto 1); zbkmtg : inout time; roxkbhofns : in time);
end cdkqjhqq;

architecture o of cdkqjhqq is
  
begin
  
end o;

entity dtwcwh is
  port (kyee : buffer time);
end dtwcwh;

architecture lzp of dtwcwh is
  signal im : time;
  signal nw : string(2 downto 1);
  signal i : time;
  signal dc : time;
  signal o : string(2 downto 1);
begin
  lfpzuuf : entity work.cdkqjhqq
    port map (myoomxnk => o, zbkmtg => dc, roxkbhofns => i);
  selbmkz : entity work.cdkqjhqq
    port map (myoomxnk => nw, zbkmtg => kyee, roxkbhofns => im);
end lzp;

library ieee;
use ieee.std_logic_1164.all;

entity gghsmnlbo is
  port (vat : in time; fmekmpr : in bit; cbiz : buffer std_logic_vector(3 to 2));
end gghsmnlbo;

architecture ykolobanv of gghsmnlbo is
  signal uyxdr : time;
  signal qef : string(2 downto 1);
begin
  ip : entity work.cdkqjhqq
    port map (myoomxnk => qef, zbkmtg => uyxdr, roxkbhofns => uyxdr);
  
  -- Multi-driven assignments
  cbiz <= cbiz;
end ykolobanv;

entity moqu is
  port (fysf : inout bit; typw : buffer severity_level);
end moqu;

library ieee;
use ieee.std_logic_1164.all;

architecture n of moqu is
  signal ivzwlloyna : std_logic_vector(3 to 2);
  signal iqzf : time;
  signal qgjyim : time;
  signal x : string(2 downto 1);
  signal reeavk : time;
  signal xtlzfoijk : time;
  signal kmbyfons : string(2 downto 1);
begin
  dawzx : entity work.cdkqjhqq
    port map (myoomxnk => kmbyfons, zbkmtg => xtlzfoijk, roxkbhofns => reeavk);
  l : entity work.cdkqjhqq
    port map (myoomxnk => x, zbkmtg => reeavk, roxkbhofns => qgjyim);
  wxdihd : entity work.dtwcwh
    port map (kyee => qgjyim);
  ggxzndg : entity work.gghsmnlbo
    port map (vat => iqzf, fmekmpr => fysf, cbiz => ivzwlloyna);
  
  -- Single-driven assignments
  typw <= typw;
  
  -- Multi-driven assignments
  ivzwlloyna <= ivzwlloyna;
  ivzwlloyna <= (others => '0');
  ivzwlloyna <= ivzwlloyna;
  ivzwlloyna <= (others => '0');
end n;



-- Seed after: 13308659757137383679,6869379746012019351
