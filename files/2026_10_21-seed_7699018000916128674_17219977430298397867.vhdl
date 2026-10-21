-- Seed: 7699018000916128674,17219977430298397867

entity uch is
  port (mtps : in string(4 downto 2));
end uch;

architecture fdmi of uch is
  
begin
  
end fdmi;

entity nxnarrg is
  port (skohbg : buffer character);
end nxnarrg;

architecture we of nxnarrg is
  signal zi : string(4 downto 2);
  signal pbur : string(4 downto 2);
  signal qamc : string(4 downto 2);
begin
  fobjskevo : entity work.uch
    port map (mtps => qamc);
  kvfzmzmb : entity work.uch
    port map (mtps => pbur);
  zx : entity work.uch
    port map (mtps => qamc);
  dgjspdtssv : entity work.uch
    port map (mtps => zi);
  
  -- Single-driven assignments
  qamc <= ('n', 'q', 'u');
end we;



-- Seed after: 6984181019388748725,17219977430298397867
