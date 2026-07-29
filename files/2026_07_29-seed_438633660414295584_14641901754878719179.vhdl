-- Seed: 438633660414295584,14641901754878719179

entity wirja is
  port (ob : in time_vector(2 downto 3));
end wirja;

architecture scr of wirja is
  
begin
  
end scr;

entity sax is
  port (syrpd : out integer; ux : in time);
end sax;

architecture vfq of sax is
  signal y : time_vector(2 downto 3);
begin
  xxdqspg : entity work.wirja
    port map (ob => y);
  xjpfnhdt : entity work.wirja
    port map (ob => y);
  
  -- Single-driven assignments
  syrpd <= syrpd;
end vfq;

entity bsbjxk is
  port (tjtttot : inout integer);
end bsbjxk;

architecture ojs of bsbjxk is
  signal hxhhinyr : time;
  signal sprpl : time_vector(2 downto 3);
begin
  vptjc : entity work.wirja
    port map (ob => sprpl);
  urwnt : entity work.sax
    port map (syrpd => tjtttot, ux => hxhhinyr);
  puegdlrsql : entity work.wirja
    port map (ob => sprpl);
  
  -- Single-driven assignments
  sprpl <= sprpl;
  hxhhinyr <= hxhhinyr;
end ojs;



-- Seed after: 13896909995334426647,14641901754878719179
