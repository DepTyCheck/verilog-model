-- Seed: 12622606042083743071,16159265764638711791

entity tjftbqeu is
  port (kjukckpb : in boolean_vector(0 downto 3); wova : out real);
end tjftbqeu;

architecture rid of tjftbqeu is
  
begin
  -- Single-driven assignments
  wova <= 16#7.0_0_7#;
end rid;

entity aqknc is
  port (qizgwuw : buffer real_vector(2 downto 1));
end aqknc;

architecture qtjdzv of aqknc is
  signal kfhpryu : real;
  signal yssegm : real;
  signal gmzg : boolean_vector(0 downto 3);
  signal dm : real;
  signal nwlgyk : boolean_vector(0 downto 3);
begin
  tnsyde : entity work.tjftbqeu
    port map (kjukckpb => nwlgyk, wova => dm);
  uadnl : entity work.tjftbqeu
    port map (kjukckpb => gmzg, wova => yssegm);
  vgbtyyifqy : entity work.tjftbqeu
    port map (kjukckpb => gmzg, wova => kfhpryu);
  
  -- Single-driven assignments
  gmzg <= (others => TRUE);
end qtjdzv;



-- Seed after: 1296121215663424561,16159265764638711791
