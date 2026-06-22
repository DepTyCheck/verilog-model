-- Seed: 12779875082022064898,13479070923501788437

entity ltsgg is
  port (bpy : in boolean_vector(0 downto 3));
end ltsgg;

architecture h of ltsgg is
  
begin
  
end h;

entity duzwxzi is
  port (wzlrsab : out real_vector(2 downto 0); bqs : linkage real);
end duzwxzi;

architecture os of duzwxzi is
  signal jrakqce : boolean_vector(0 downto 3);
begin
  egvxkudn : entity work.ltsgg
    port map (bpy => jrakqce);
  yjtbyot : entity work.ltsgg
    port map (bpy => jrakqce);
  cl : entity work.ltsgg
    port map (bpy => jrakqce);
  
  -- Single-driven assignments
  wzlrsab <= (8#0026.6#, 16#F_9_F_B_8.735#, 2#11.0#);
  jrakqce <= (others => TRUE);
end os;



-- Seed after: 1394365649014700202,13479070923501788437
